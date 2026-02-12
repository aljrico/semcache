test_that("cache persists across sessions", {
  path <- tempfile("semcache_test_")
  on.exit(unlink(path, recursive = TRUE, force = TRUE))

  cache <- sc_open(path, semantic = "off")
  sc_put(cache, "persistent query", "persistent answer")
  expect_equal(nrow(cache$meta), 1L)

  # Simulate closing session
  rm(cache)
  gc()

  # Reopen
  cache2 <- sc_open(path, semantic = "off")

  expect_equal(nrow(cache2$meta), 1L)
  result <- sc_get(cache2, "persistent query", mode = "exact")
  expect_true(result$hit)
  expect_equal(result$entry$response, "persistent answer")
})

test_that("multiple entries persist", {
  path <- tempfile("semcache_test_")
  on.exit(unlink(path, recursive = TRUE, force = TRUE))

  cache <- sc_open(path, semantic = "off")
  sc_put(cache, "q1", "a1")
  sc_put(cache, "q2", "a2")
  sc_put(cache, "q3", "a3")

  rm(cache)
  gc()

  cache2 <- sc_open(path, semantic = "off")

  expect_equal(nrow(cache2$meta), 3L)
  expect_true(sc_get(cache2, "q1", mode = "exact")$hit)
  expect_true(sc_get(cache2, "q2", mode = "exact")$hit)
  expect_true(sc_get(cache2, "q3", mode = "exact")$hit)
})

test_that("config persists", {
  path <- tempfile("semcache_test_")
  on.exit(unlink(path, recursive = TRUE, force = TRUE))

  cache <- sc_open(path, semantic = "off", strict = FALSE)

  rm(cache)
  gc()

  cache2 <- sc_open(path, semantic = "off")

  expect_false(cache2$config$strict)
})
