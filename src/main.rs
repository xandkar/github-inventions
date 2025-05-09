use std::path::PathBuf;

use clap::Parser;
use tracing::level_filters::LevelFilter;

#[derive(Parser, Debug)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::ERROR)]
    log_level: LevelFilter,

    /// Use cached data from GitHub, if it is available.
    #[clap(short, long)]
    cache: bool,

    /// How shall we identify ourselves to the GitHub API server?
    #[clap(short, long, default_value = "Tormozilla/2")]
    user_agent: String,

    #[clap(short, long)]
    exclude: Vec<String>,

    #[clap(short, long = "desc-width", default_value_t = 50)]
    description_width: u16,

    #[clap(short, long = "out")]
    output_file: Option<PathBuf>,

    /// GitHub username.
    user_name: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    tracing_init(cli.log_level)?;
    let span = tracing::debug_span!(github_inventions::crate_name!());
    let _span_guard = span.enter();
    tracing::info!(?cli, "Starting.");
    github_inventions::list(
        &cli.user_name,
        &cli.user_agent,
        cli.cache,
        &cli.exclude[..],
        cli.description_width,
        cli.output_file.as_deref(),
    )?;
    Ok(())
}

fn tracing_init(level: LevelFilter) -> anyhow::Result<()> {
    use tracing_subscriber::{
        fmt::{self, format::FmtSpan},
        layer::SubscriberExt,
        EnvFilter, Layer,
    };

    let span_events = if let Some(tracing::Level::TRACE) = level.into_level()
    {
        FmtSpan::NEW | FmtSpan::CLOSE
    } else {
        FmtSpan::CLOSE
    };

    let layer_stderr = fmt::Layer::new()
        .with_writer(std::io::stderr)
        .with_ansi(true)
        .with_file(false)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_span_events(span_events)
        .with_filter(
            EnvFilter::from_default_env().add_directive(level.into()),
        );
    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(layer_stderr),
    )?;
    Ok(())
}
