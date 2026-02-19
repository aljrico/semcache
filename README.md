# semcache <img src="man/figures/logo.svg" align="right" height="139" alt="semcache logo" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/semcache)](https://cran.r-project.org/package=semcache)
<!-- badges: end -->

**Local, persistent, auditable cache for LLM calls** — with optional
semantic matching.

Every call you make to an LLM costs time and money. `semcache` saves both
by caching request/response pairs on disk. Exact matches are instant.
Optional semantic matching means *"What is the capital of France?"* and
*"Tell me France's capital city"* can share the same cached answer — without
a vector database, without a server, without any setup beyond
`sc_open("my_cache")`.

Every cached entry lives on disk in a readable file. You get a free audit
log of every prompt and response, portable across machines and sessions.

## Installation

```r
# Development version
# install.packages("pak")
pak::pak("aljrico/semcache")

# Or with remotes
remotes::install_github("aljrico/semcache")
```

**Optional dependencies:**

| Package | Purpose | Required? |
|---|---|---|
| `usearchlite` | Semantic (embedding) search | No — exact mode works without it |
| `toonlite` | Alternative blob storage format | No — RDS is the default |
| `jsonlite` | Pretty JSON for tags/cost metadata | No — minimal fallback included |

## Quickstart: exact caching

```r
library(semcache)

cache <- sc_open(tempdir())

# Simulate an LLM call
fake_llm <- function(prompt) {
  Sys.sleep(0.5)
  paste0("Answer to: ", prompt)
}

# Wrap it
cached_llm <- sc_wrap(cache, fake_llm, mode = "exact")

# First call: 500ms (calls the real function)
system.time(cached_llm("What is 2+2?"))
#>    user  system elapsed
#>   0.001   0.000   0.502

# Second call: instant (served from cache)
system.time(cached_llm("What is 2+2?"))
#>    user  system elapsed
#>   0.001   0.000   0.001
```

## Semantic caching

This is the sales moment. Two different prompts that mean the same thing?
One LLM call, not two.

```r
library(semcache)

cache <- sc_open(tempfile(), semantic = "auto", dim = 3L)

# A stub embedder (in practice, use an embedding API)
embed <- function(text) {
  if (grepl("capital.*France|France.*capital", text))
    return(c(0.95, 0.05, 0.00))
  if (grepl("weather|rain|sunny", text))
    return(c(0.00, 0.10, 0.90))
  c(0.33, 0.33, 0.34)
}

# First query
sc_put(cache, "What is the capital of France?", "Paris",
       embedding = embed("What is the capital of France?"))

# Slightly different phrasing
result <- sc_get(cache, "Tell me France's capital city",
                 mode = "semantic",
                 embedding = embed("Tell me France's capital city"),
                 threshold = 0.90)

result$hit
#> [1] TRUE
result$entry$response
#> [1] "Paris"
result$score
#> [1] 0.9975
```

Semantic mode requires the `usearchlite` package. If it is not installed,
`semcache` works in exact-only mode.

### Strict mode prevents wrong reuse

By default, `semcache` requires that semantically matched entries share the
same **model**, **tool schema**, and **temperature bucket**. This prevents a
GPT-4 answer from being reused for a GPT-3.5 query.
Set `strict = FALSE` to relax these guards when you know it is safe.

## Wrapping any LLM caller

`semcache` does not depend on any LLM provider package. Wrap whatever
function you use:

```r
library(semcache)

cache <- sc_open("~/.my_llm_cache")

# Your function that calls an LLM (any provider)
my_llm_call <- function(prompt, model = "gpt-4") {
  # ... your API call here ...
}

cached_call <- sc_wrap(
  cache, my_llm_call,
  mode = "exact",
  key_fn = function(prompt, model = "gpt-4") {
    list(model = model, messages = list(list(role = "user", content = prompt)))
  }
)

# Now just use cached_call() instead of my_llm_call()
```

## Audit log & inspection

Every cache entry is a file on disk you can read, copy, or grep.

```r
cache <- sc_open("my_cache")

# Overview
sc_stats(cache)
#> $entries
#> [1] 42
#> $hits
#> [1] 0
#> $misses
#> [1] 0
#> $bytes
#> [1] 156800

# Read any entry
entry <- sc_read(cache, 1L)
entry$request   # the canonicalized request
entry$response  # the stored response
entry$meta      # timestamps, cost, tags, timing
```

The blobs directory contains one file per entry:

```
my_cache/
  config.rds
  meta.rds
  blobs/
    000001.rds
    000002.rds
    ...
```

## Safety & defaults

| Setting | Default | What it does |
|---|---|---|
| `strict` | `TRUE` | Semantic reuse requires matching model, tools, temperature bucket |
| `threshold` | `0.92` | Minimum cosine similarity for semantic hit |
| `mode` | `"both"` | Try exact first, fall back to semantic |
| `allow_stale` | `FALSE` | Expired entries are skipped |
| `ttl` | `Inf` | Entries never expire unless you set a TTL |
| `version` | `"v1"` | Entries from different versions never match |

**Temperature bucketing:** temperatures are rounded to the nearest 0.2
(so 0.69 and 0.71 both map to 0.6). This prevents trivial float
differences from busting the cache.

**Versioning:** bump the `version` string when you change your prompt
template. Old cached answers will not be reused.

## Cache management

```r
# Remove expired entries
sc_prune(cache)

# Keep only the most recent 1000 entries
sc_prune(cache, max_entries = 1000)

# Preview what would be removed
sc_prune(cache, max_entries = 500, dry_run = TRUE)
```

## Comparison

| Feature | `memoise` / `digest` | Provider caching | `semcache` |
|---|---|---|---|
| Exact match | Yes | Yes | Yes |
| Semantic match | No | No | Yes (opt-in) |
| Persistent across sessions | File-backed | No (server-side) | Yes |
| Auditable on disk | Not designed for it | No | Yes |
| Provider-independent | Depends | No | Yes |
| TTL / pruning | Limited | Provider-controlled | Yes |

## Why local-first?

- **CI/CD:** cache lives next to your code; no external service to configure.
- **Privacy:** prompts and responses never leave your machine.
- **Reproducibility:** pin a cache directory and get identical results across
  runs without making API calls.
- **Cost:** no database, no server, no subscription.

## What semcache does NOT do

- It is not an LLM client. It caches the results of *your* LLM calls.
- It does not generate embeddings. You provide them (or skip semantic mode).
- It is not a vector database. The semantic index is a lightweight ANN index
  for cache lookup, not a general-purpose search engine.

## Contributing & roadmap

Contributions welcome. Please open an issue before large PRs.

**v0.1** (current): exact + semantic caching, RDS storage, pruning.
**v0.2** (planned): toon backend improvements, disk compaction, richer
pruning policies.
