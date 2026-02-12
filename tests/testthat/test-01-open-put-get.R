test_that("sc_open creates directory structure", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  expect_s3_class(cache, "semcache")
  expect_true(dir.exists(path))
  expect_true(dir.exists(file.path(path, "blobs")))
  expect_true(file.exists(file.path(path, "config.rds")))
  expect_equal(nrow(cache$meta), 0L)
  expect_false(cache$semantic)
})

test_that("sc_fingerprint canonicalizes character input", {
  fp <- sc_fingerprint("hello")
  expect_true(is.list(fp))
  expect_equal(fp$messages[[1]]$role, "user")
  expect_equal(fp$messages[[1]]$content, "hello")
  expect_equal(attr(fp, "semcache_version"), "v1")
})

test_that("sc_fingerprint sorts named lists", {
  req <- list(model = "gpt-4", params = list(z = 1, a = 2), messages = list())
  fp <- sc_fingerprint(req)
  expect_equal(names(fp$params), c("a", "z"))
})

test_that("sc_key produces stable hashes", {
  k1 <- sc_key("hello")
  k2 <- sc_key("hello")
  expect_equal(k1$key_hash, k2$key_hash)

  k3 <- sc_key("goodbye")
  expect_false(k1$key_hash == k3$key_hash)
})

test_that("sc_put and sc_get exact round-trip", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "What is 2+2?", "4")
  result <- sc_get(cache, "What is 2+2?", mode = "exact")

  expect_true(result$hit)
  expect_equal(result$mode, "exact")
  expect_equal(result$entry$response, "4")
})

test_that("sc_put updates meta correctly", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "test prompt", "test response",
         tags = list(project = "demo"),
         cost = list(prompt_tokens = 10, completion_tokens = 5,
                     total_tokens = 15),
         timing_ms = 42.5)

  expect_equal(nrow(cache$meta), 1L)
  expect_equal(cache$meta$id[1L], 1L)
  expect_equal(cache$meta$timing_ms[1L], 42.5)
  expect_true(cache$meta$ttl_seconds[1L] == Inf)
})

test_that("sc_get returns miss for unknown request", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  result <- sc_get(cache, "unknown", mode = "exact")
  expect_false(result$hit)
  expect_true(is.na(result$mode))
})

test_that("TTL expiration works", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  # Store with TTL=1 second but set created_at in the past
  past <- Sys.time() - 100
  sc_put(cache, "expiring", "gone", ttl = 1, created_at = past)

  result <- sc_get(cache, "expiring", mode = "exact")
  expect_false(result$hit)

  # allow_stale should still find it
  result2 <- sc_get(cache, "expiring", mode = "exact", allow_stale = TRUE)
  expect_true(result2$hit)
})

test_that("sc_read retrieves entry by ID", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "q1", "a1")
  entry <- sc_read(cache, 1L)
  expect_equal(entry$response, "a1")
  expect_equal(entry$id, 1L)
})

test_that("sc_read errors on unknown ID", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  expect_error(sc_read(cache, 999L), "not found")
})

test_that("sc_stats returns correct structure", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "q", "a")
  stats <- sc_stats(cache)
  expect_equal(stats$entries, 1L)
  expect_false(stats$semantic)
  expect_true(stats$bytes > 0)
})

test_that("print.semcache works", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  expect_output(print(cache), "semcache")
})

test_that("list response is stored and retrieved", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  resp <- list(text = "hello", usage = list(tokens = 10))
  sc_put(cache, "q", resp)
  result <- sc_get(cache, "q", mode = "exact")
  expect_true(result$hit)
  expect_equal(result$entry$response$text, "hello")
})
