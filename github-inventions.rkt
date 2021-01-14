#! /usr/bin/env racket

#lang racket

(require net/http-client)

(require json)
(require gregor)

(define/contract (repos-fetch username)
  (-> string? jsexpr?)
  (define uri-domain "api.github.com")
  (define uri-path
    (string-append "/users/" username "/repos?page=1&per_page=10000"))
  (define-values (status-line headers data-port)
    (http-sendrecv
      uri-domain
      uri-path
      #:ssl? #t))
  (define status (string-split (bytes->string/utf-8 status-line)))
  (define status-code (string->number (second status)))
  (if (= 200 status-code)
      (read-json data-port)
      (raise (format "[error] fetch failed with ~a\n" status-code))))

(define/contract (main username)
  (-> string? void?)
  (let* ([repos (repos-fetch username)]
         [repos (filter (λ (r) (not (dict-ref r 'fork))) repos)]
         [repos (map (λ (r) (list (dict-ref r 'name)
                                  (iso8601->date (dict-ref r 'created_at))
                                  (dict-ref r 'description))) repos)]
         [repos (sort repos (λ (a b) (date>? (second a) (second b))))]
         [repos (map (match-lambda
                       [(list name date descr)
                        (list name (date->iso8601 date) (if (string? descr) descr "-"))])
                     repos)]
         [repos (map (λ (r) (string-join r "|")) repos)])
        (for-each displayln repos)))

(module+ main
         (command-line #:args (username) (main username)))
