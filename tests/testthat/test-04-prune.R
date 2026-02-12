test_that("prune removes expired entries", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  past <- Sys.time() - 100
  sc_put(cache, "expired", "gone", ttl = 1, created_at = past)
  sc_put(cache, "fresh", "here")

  expect_equal(nrow(cache$meta), 2L)

  result <- sc_prune(cache)
  expect_equal(result$n_removed, 1L)
  expect_equal(nrow(cache$meta), 1L)
  expect_false(sc_get(cache, "expired", mode = "exact")$hit)
  expect_true(sc_get(cache, "fresh", mode = "exact")$hit)
})

test_that("prune dry_run does not delete", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  past <- Sys.time() - 100
  sc_put(cache, "expired", "gone", ttl = 1, created_at = past)

  result <- sc_prune(cache, dry_run = TRUE)
  expect_equal(result$n_removed, 1L)
  # Entry still exists
  expect_equal(nrow(cache$meta), 1L)
})

test_that("prune max_entries works", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  for (i in 1:5) {
    sc_put(cache, paste0("q", i), paste0("a", i),
           created_at = Sys.time() + i)
  }
  expect_equal(nrow(cache$meta), 5L)

  result <- sc_prune(cache, max_entries = 3L)
  expect_equal(nrow(cache$meta), 3L)
  # Oldest should be removed
  expect_equal(result$n_removed, 2L)
})

test_that("prune older_than works", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  old_time <- Sys.time() - 3600
  sc_put(cache, "old", "old-answer", created_at = old_time)
  sc_put(cache, "new", "new-answer")

  result <- sc_prune(cache, older_than = Sys.time() - 1800)
  expect_equal(result$n_removed, 1L)
  expect_false(sc_get(cache, "old", mode = "exact")$hit)
  expect_true(sc_get(cache, "new", mode = "exact")$hit)
})

test_that("prune cleans up blobs on disk", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  past <- Sys.time() - 100
  sc_put(cache, "expired", "gone", ttl = 1, created_at = past)

  blobs_before <- list.files(file.path(path, "blobs"))
  expect_length(blobs_before, 1L)

  sc_prune(cache)

  blobs_after <- list.files(file.path(path, "blobs"))
  expect_length(blobs_after, 0L)
})
