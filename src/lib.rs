use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, bail, Context};
use chrono::{DateTime, Utc};

#[macro_export]
macro_rules! crate_name {
    () => {
        env!("CARGO_PKG_NAME")
    };
}

pub fn list(
    user_name: &str,
    user_agent: &str,
    cache: bool,
    exclude: &[String],
    description_width: u16,
    output_file: Option<&Path>,
) -> anyhow::Result<()> {
    let mut repos = if cache {
        let repos =
            cache_get(user_name)?.ok_or(anyhow!("Cached data not found."))?;
        tracing::debug!(repo_count = repos.len(), "Retrieved from cache.");
        repos
    } else {
        let repos = fetch(user_name, user_agent)?;
        tracing::debug!(repo_count = repos.len(), "Fetched from GitHub.");
        cache_set(user_name, &repos[..])?;
        repos
    };
    repos.retain(|r| !r.fork);
    tracing::debug!(repo_count = repos.len(), "Removed forks.");

    let exclude: HashSet<String> = exclude.iter().cloned().collect();
    repos.retain(|r| !exclude.contains(&r.name));
    tracing::debug!(repo_count = repos.len(), "Removed excluded.");

    repos.sort_by_key(|r| r.created_at); // Oldest on top.
    repos.reverse(); // Youngest on top.
    tracing::debug!(
        first = ?repos.first().map(|r| (&r.name, r.created_at)),
        last = ?repos.last().map(|r| (&r.name, r.created_at)),
        "Sorted."
    );
    output_text_table(&repos[..], description_width, output_file)?;
    Ok(())
}

fn output_text_table(
    repos: &[Repo],
    description_width: u16,
    output_file: Option<&Path>,
) -> anyhow::Result<()> {
    use comfy_table::{
        presets, ColumnConstraint, ContentArrangement, Table, Width,
    };

    let mut table = Table::new();
    table
        .load_preset(presets::NOTHING) // No borders or dividers.
        .set_content_arrangement(ContentArrangement::Dynamic)
        // .set_width(80)
        .set_header(["NAME", "CREATED_ON", "DESCRIPTION"])
        .set_constraints(vec![
            ColumnConstraint::ContentWidth,
            ColumnConstraint::ContentWidth,
            ColumnConstraint::UpperBoundary(Width::Fixed(description_width)),
        ]);
    table.column_iter_mut().for_each(|column| {
        column.set_padding((0, 2));
    });

    for Repo {
        name,
        created_at,
        description,
        ..
    } in repos
    {
        let created_at: String = created_at.date_naive().to_string();
        let created_at: &str = created_at.as_str();
        let description: &str = description.as_deref().unwrap_or_default();

        // Separator row.
        table.add_row(vec![""; 3]);

        table.add_row(vec![name, created_at, description]);
    }
    let table = table.to_string();
    if let Some(output_file) = output_file {
        fs::write(output_file, &table)?;
    } else {
        println!("{table}");
    }
    Ok(())
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct Repo {
    name: String,
    description: Option<String>,
    // html_url: String,
    created_at: DateTime<Utc>,
    fork: bool,
}

fn fetch(user_name: &str, user_agent: &str) -> anyhow::Result<Vec<Repo>> {
    let mut repos = Vec::new();
    let mut page: usize = 0;
    let client = reqwest::blocking::Client::new();
    loop {
        page += 1;
        let url = format!(
            "https://api.github.com/users/{}/repos?page={}&per_page=25",
            user_name, page
        );
        let resp = client
            .get(&url)
            .header("User-Agent", user_agent)
            .send()
            .context(format!("Failed to send request to: {url:?}"))?;
        if !resp.status().is_success() {
            bail!(
                "Failed response for request to {url:?}. Response: {resp:?}"
            )
        }
        let body =
            resp.text().context("Failed to get response body text.")?;
        let mut batch: Vec<Repo> = serde_json::from_str(&body)
            .context("Failed to deserialize response body.")?;
        tracing::debug!(page, size = batch.len(), "Fetched repos batch.");
        if batch.is_empty() {
            break;
        }
        repos.append(&mut batch);
    }
    Ok(repos)
}

fn cache_get(username: &str) -> anyhow::Result<Option<Vec<Repo>>> {
    let file_path = cache_file_path(username)?;
    if file_path.try_exists()? {
        let data = fs::read_to_string(&file_path)?;
        let repos = serde_json::from_str(&data)?;
        Ok(Some(repos))
    } else {
        Ok(None)
    }
}

fn cache_set(username: &str, repos: &[Repo]) -> anyhow::Result<()> {
    let file_path = cache_file_path(username)?;
    if let Some(parent_dir) = file_path.parent() {
        fs::create_dir_all(parent_dir)
            .context(format!("Failed to create directory: {parent_dir:?}"))?;
    }
    let data = serde_json::to_string_pretty(repos)?;
    fs::write(file_path, &data)?;
    Ok(())
}

fn cache_file_path(username: &str) -> anyhow::Result<PathBuf> {
    let path = dirs::cache_dir()
        .ok_or(anyhow!("System cache directory could not be determined."))?
        .join(crate_name!())
        .join("repos");
    let path = path.join(username).with_extension("json");
    Ok(path)
}
