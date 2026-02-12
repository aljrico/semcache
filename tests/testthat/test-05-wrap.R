test_that("sc_wrap caches function results", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  call_count <- 0L
  my_fn <- function(prompt) {
    call_count <<- call_count + 1L
    paste0("answer:", prompt)
  }

  wrapped <- sc_wrap(cache, my_fn, mode = "exact")

  # First call: miss -> calls underlying function
  r1 <- wrapped("hello")
  expect_equal(r1, "answer:hello")
  expect_equal(call_count, 1L)

  # Second call: hit -> does NOT call underlying function
  r2 <- wrapped("hello")
  expect_equal(r2, "answer:hello")
  expect_equal(call_count, 1L)
})

test_that("sc_wrap different args get different results", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  call_count <- 0L
  my_fn <- function(prompt) {
    call_count <<- call_count + 1L
    paste0("answer:", prompt)
  }

  wrapped <- sc_wrap(cache, my_fn, mode = "exact")

  r1 <- wrapped("hello")
  r2 <- wrapped("world")
  expect_equal(call_count, 2L)
  expect_equal(r1, "answer:hello")
  expect_equal(r2, "answer:world")
})

test_that("sc_wrap with custom key_fn", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  call_count <- 0L
  my_fn <- function(x, y) {
    call_count <<- call_count + 1L
    x + y
  }

  wrapped <- sc_wrap(cache, my_fn, mode = "exact",
                     key_fn = function(x, y) list(x = x, y = y))

  r1 <- wrapped(1, 2)
  expect_equal(r1, 3)
  expect_equal(call_count, 1L)

  r2 <- wrapped(1, 2)
  expect_equal(r2, 3)
  expect_equal(call_count, 1L)
})

test_that("sc_wrap records errors when record_errors=TRUE", {
  path <- tempfile("semcache_test_")
  cache <- sc_open(path, semantic = "off")
  on.exit({
    rm(cache)
    gc()
    unlink(path, recursive = TRUE, force = TRUE)
  })

  call_count <- 0L
  failing_fn <- function(prompt) {
    call_count <<- call_count + 1L
    stop("boom")
  }

  wrapped <- sc_wrap(cache, failing_fn, mode = "exact",
                     record_errors = TRUE)

  r1 <- wrapped("trigger")
  expect_true(inherits(r1, "error"))
  expect_equal(call_count, 1L)

  # Second call returns cached error without calling fn again
  r2 <- wrapped("trigger")
  expect_true(inherits(r2, "error"))
  expect_equal(call_count, 1L)
})
