#' Wrap a Function with Cache
#'
#' Returns a new function that checks the cache before calling `fn`. On a
#' cache miss the underlying function is called, and the result is stored.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @param fn The function to wrap.
#' @param mode Cache lookup mode: `"both"`, `"exact"`, or `"semantic"`.
#' @param embed_fn Optional function that takes the same arguments as `fn` and
#'   returns an embedding vector. Used for semantic put/get.
#' @param key_fn Optional function that takes `...` and returns a request list
#'   suitable for [sc_fingerprint()]. If `NULL`, the arguments are captured as
#'   a named list.
#' @param record_errors If `TRUE` (default), errors from `fn` are also cached
#'   (wrapped in a `simpleError`).
#' @return A wrapped function with the same interface as `fn`.
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' my_fn <- function(prompt) paste0("answer:", prompt)
#' cached_fn <- sc_wrap(cache, my_fn, mode = "exact")
#' cached_fn("hello")
#' cached_fn("hello")  # served from cache
#' }
#' @export
sc_wrap <- function(cache,
                    fn,
                    mode = c("both", "exact", "semantic"),
                    embed_fn = NULL,
                    key_fn = NULL,
                    record_errors = TRUE) {
  stopifnot(inherits(cache, "semcache"))
  mode <- match.arg(mode)
  force(fn)
  force(embed_fn)
  force(key_fn)
  force(record_errors)

  function(...) {
    # Build request
    request <- if (!is.null(key_fn)) {
      key_fn(...)
    } else {
      args <- list(...)
      if (length(args) == 1L && is.character(args[[1L]])) {
        args[[1L]]
      } else {
        args
      }
    }

    # Compute embedding if available
    emb <- if (!is.null(embed_fn)) embed_fn(...) else NULL

    # Check cache
    result <- sc_get(cache, request, mode = mode, embedding = emb)
    if (result$hit) {
      return(result$entry$response)
    }

    # Call underlying function
    t0 <- proc.time()[["elapsed"]]
    if (record_errors) {
      response <- tryCatch(fn(...), error = function(e) e)
    } else {
      response <- fn(...)
    }
    elapsed <- (proc.time()[["elapsed"]] - t0) * 1000

    # Store
    sc_put(cache, request, response, embedding = emb, timing_ms = elapsed)
    response
  }
}
