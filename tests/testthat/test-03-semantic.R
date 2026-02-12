skip_if_not_installed("usearchlite")

test_that("semantic cache basic workflow", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "on", dim = 3L)
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  expect_true(cache$semantic)

  # Store 3 orthogonal embeddings
  sc_put(cache, "x-axis", "response-x", embedding = c(1, 0, 0))
  sc_put(cache, "y-axis", "response-y", embedding = c(0, 1, 0))
  sc_put(cache, "z-axis", "response-z", embedding = c(0, 0, 1))

  # Query near x-axis
  result <- sc_get(cache, "near-x", mode = "semantic",
                   embedding = c(0.95, 0.05, 0),
                   threshold = 0.9)
  expect_true(result$hit)
  expect_equal(result$mode, "semantic")
  expect_equal(result$entry$response, "response-x")
  expect_true(result$score > 0.9)
})

test_that("semantic strict mode filters by model", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "on", dim = 3L, strict = TRUE)
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  # Two entries with same embedding but different models
  req_a <- list(model = "gpt-4", messages = list(list(role = "user",
                                                       content = "hello")))
  req_b <- list(model = "gpt-3.5", messages = list(list(role = "user",
                                                         content = "hello")))
  sc_put(cache, req_a, "answer-gpt4", embedding = c(1, 0, 0))
  sc_put(cache, req_b, "answer-gpt35", embedding = c(1, 0, 0))

  # Query with model=gpt-4 should only return the gpt-4 entry
  query_req <- list(model = "gpt-4",
                    messages = list(list(role = "user", content = "hi")))
  result <- sc_get(cache, query_req, mode = "semantic",
                   embedding = c(1, 0, 0), threshold = 0.9, strict = TRUE)
  expect_true(result$hit)
  expect_equal(result$entry$response, "answer-gpt4")
})

test_that("semantic threshold causes miss", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "on", dim = 3L, strict = FALSE)
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "x-axis", "response-x", embedding = c(1, 0, 0))

  # Query with very different embedding and high threshold
  result <- sc_get(cache, "far-away", mode = "semantic",
                   embedding = c(0, 1, 0), threshold = 0.99)
  expect_false(result$hit)
})

test_that("semantic cache persists across sessions", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "on", dim = 3L)

  sc_put(cache, "q", "a", embedding = c(1, 0, 0))

  rm(cache)
  gc()

  cache2 <- sc_open(path, semantic = "on", dim = 3L)
  on.exit({
    rm(cache2)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  result <- sc_get(cache2, "near-q", mode = "semantic",
                   embedding = c(0.99, 0.01, 0), threshold = 0.9)
  expect_true(result$hit)
})

test_that("semantic=on errors without usearchlite mock", {
  # This test verifies the error path; since usearchlite IS installed,
 # we just verify that semantic=on with missing dim errors.
  path <- tempfile("semcache_test_")
  on.exit(unlink(path, recursive = TRUE, force = TRUE))
  expect_error(
    sc_open(path, semantic = "on"),
    "dimension"
  )
})

test_that("sc_get semantic requires embedding", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "on", dim = 3L)
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  sc_put(cache, "q", "a", embedding = c(1, 0, 0))
  expect_error(
    sc_get(cache, "q2", mode = "semantic"),
    "embedding"
  )
})
