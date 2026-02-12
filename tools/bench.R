#!/usr/bin/env Rscript
# semcache benchmarks
# Run: Rscript tools/bench.R

library(semcache)

cat("=== semcache benchmarks ===\n\n")

# --- Setup ---
path <- tempfile("semcache_bench_")
on.exit(unlink(path, recursive = TRUE, force = TRUE))

n_exact <- 1000L
n_semantic <- 1000L

has_bench <- requireNamespace("bench", quietly = TRUE)
has_usearch <- requireNamespace("usearchlite", quietly = TRUE)

time_it <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  ops <- n_exact / elapsed
  cat(sprintf("  %-35s %7.2f ms  (%6.0f ops/sec)\n",
              label, elapsed * 1000, ops))
  elapsed
}

results <- data.frame(
  test = character(), n = integer(), time_ms = numeric(),
  ops_sec = numeric(), stringsAsFactors = FALSE
)
add_result <- function(test, n, time_s) {
  results <<- rbind(results, data.frame(
    test = test, n = n, time_ms = round(time_s * 1000, 2),
    ops_sec = round(n / time_s), stringsAsFactors = FALSE
  ))
}

# --- Exact caching ---
cat("Exact caching (RDS backend)\n")
cache <- sc_open(path, semantic = "off", storage = "rds")

# Benchmark: sc_put
prompts <- paste0("prompt_", seq_len(n_exact))
t <- time_it(paste0("sc_put x", n_exact), {
  for (i in seq_len(n_exact)) {
    sc_put(cache, prompts[i], paste0("response_", i))
  }
})
add_result("exact_put", n_exact, t)

# Benchmark: sc_get (all hits)
t <- time_it(paste0("sc_get exact hit x", n_exact), {
  for (i in seq_len(n_exact)) {
    res <- sc_get(cache, prompts[i], mode = "exact")
    stopifnot(res$hit)
  }
})
add_result("exact_get_hit", n_exact, t)

# Benchmark: sc_get (all misses)
t <- time_it(paste0("sc_get exact miss x", n_exact), {
  for (i in seq_len(n_exact)) {
    res <- sc_get(cache, paste0("miss_", i), mode = "exact")
  }
})
add_result("exact_get_miss", n_exact, t)

sz <- sum(file.info(list.files(path, recursive = TRUE, full.names = TRUE))$size)
cat(sprintf("  Cache size on disk: %.1f KB (%d entries)\n\n", sz / 1024, n_exact))

rm(cache); gc(verbose = FALSE)
unlink(path, recursive = TRUE, force = TRUE)

# --- Semantic caching ---
if (has_usearch) {
  cat("Semantic caching\n")
  path_sem <- tempfile("semcache_bench_sem_")

  dim <- 128L
  cache_sem <- sc_open(path_sem, semantic = "on", dim = dim, storage = "rds")

  # Generate random embeddings
  set.seed(42)
  embeddings <- matrix(rnorm(n_semantic * dim), nrow = n_semantic)
  # Normalize to unit vectors
  embeddings <- embeddings / sqrt(rowSums(embeddings^2))

  t <- time_it(paste0("sc_put + embedding x", n_semantic), {
    for (i in seq_len(n_semantic)) {
      sc_put(cache_sem, paste0("sem_prompt_", i), paste0("sem_response_", i),
             embedding = embeddings[i, ])
    }
  })
  add_result("semantic_put", n_semantic, t)

  # Query with same embeddings (should get perfect hits)
  t <- time_it(paste0("sc_get semantic hit x", n_semantic), {
    for (i in seq_len(n_semantic)) {
      res <- sc_get(cache_sem, paste0("sem_query_", i), mode = "semantic",
                    embedding = embeddings[i, ], threshold = 0.99)
    }
  })
  add_result("semantic_get", n_semantic, t)

  sz2 <- sum(file.info(
    list.files(path_sem, recursive = TRUE, full.names = TRUE)
  )$size)
  cat(sprintf("  Cache size on disk: %.1f KB (%d entries, dim=%d)\n\n",
              sz2 / 1024, n_semantic, dim))

  rm(cache_sem); gc(verbose = FALSE)
  unlink(path_sem, recursive = TRUE, force = TRUE)
} else {
  cat("Semantic caching: SKIPPED (usearchlite not installed)\n\n")
}

# --- Summary ---
cat("=== Results ===\n")
print(results, row.names = FALSE)

# Write CSV if tools/ dir is writable
csv_path <- file.path("tools", "bench-results.csv")
tryCatch({
  write.csv(results, csv_path, row.names = FALSE)
  cat(sprintf("\nResults written to %s\n", csv_path))
}, error = function(e) {
  cat("\n(Could not write CSV)\n")
})
