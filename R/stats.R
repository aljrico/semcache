#' Cache Statistics
#'
#' Returns a summary of cache state including entry count, disk usage,
#' hit/miss counts, and semantic configuration.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @return A list with components `entries`, `hits`, `misses`, `bytes`,
#'   `pct_expired`, `semantic`, `dim`, `storage_backend`, and `version`.
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' sc_put(cache, "hello", "world")
#' sc_stats(cache)
#' }
#' @export
sc_stats <- function(cache) {
  stopifnot(inherits(cache, "semcache"))
  meta <- cache$meta
  n <- nrow(meta)
  n_expired <- if (n > 0L) {
    sum(vapply(meta$expires_at, is_expired, logical(1L)))
  } else {
    0L
  }
  list(
    entries = n,
    hits = cache$hits,
    misses = cache$misses,
    bytes = dir_size(cache$path),
    pct_expired = if (n > 0L) n_expired / n * 100 else 0,
    semantic = cache$semantic,
    dim = if (cache$semantic) cache$config$dim else NA_integer_,
    storage_backend = cache$config$storage_backend,
    version = cache$config$version
  )
}
