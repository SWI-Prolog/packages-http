# SWI-Prolog HTTP support library

This directory provides  the  SWI-Prolog   libraries  for  accessing and
providing HTTP services.

## Client library

The main client library is `library(http/http_open)`, which can open
both HTTP and HTTPS connections and handle all request methods.

## Server library

The main server libarys are

  - `library(http/thread_httpd)` implements the server
  - `library(http/http_dispatch)` implements binding locations
    predicates
  - `library(http/http_unix_daemon)` implements integration in various
    Unix server managers and in general provides a suitable entry point
    for HTTP servers on Unix.
  - `library(http/html_write)` implements generating HTML
  - `library(http/http_json)` implements reading and writing JSON documents.

# Requirements

This library uses functionality from the `ssl` package to support HTTPS,
the `sgml` package to read XML/HTML and   the `clib` package for various
extensions.
