#' Read a Cached Entry by ID
#'
#' Retrieves a stored blob entry by its integer ID.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @param id Integer entry ID.
#' @return The stored entry list (with `id`, `request`, `response`,
#'   `embedding`, `meta` components).
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' sc_put(cache, "hello", "world")
#' sc_read(cache, 1L)
#' }
#' @export
sc_read <- function(cache, id) {
  stopifnot(inherits(cache, "semcache"))
  row <- cache$meta[cache$meta$id == id, , drop = FALSE]
  if (nrow(row) == 0L) stop("Entry not found: id = ", id)
  read_blob(cache, row$blob_ref[1L], row$storage[1L])
}
