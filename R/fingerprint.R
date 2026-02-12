#' Canonicalize a Request for Caching
#'
#' Creates a stable, canonical representation of a request suitable for hashing
#' and storage. Named lists are sorted recursively by name (except message order
#' is preserved). NULL fields are removed.
#'
#' @param request A list shaped like
#'   `list(model=..., messages=..., params=list(...), tools=...)`, or a
#'   character string (wrapped as a single user message).
#' @param version Cache version string (default `"v1"`).
#' @return A canonical list with attribute `"semcache_version"`.
#' @examples
#' sc_fingerprint("What is 2+2?")
#' sc_fingerprint(list(model = "gpt-4", messages = list(
#'   list(role = "user", content = "hello")
#' )))
#' @export
sc_fingerprint <- function(request, version = "v1") {
  if (is.character(request) && length(request) == 1L) {
    request <- list(
      messages = list(list(role = "user", content = request))
    )
  }
  stopifnot(is.list(request))
  canonical <- canonicalize_recursive(request)
  attr(canonical, "semcache_version") <- version
  canonical
}

#' Compute Cache Key from a Request
#'
#' Produces deterministic hash keys from a canonicalized request. Uses
#' [digest::digest()] with the `xxhash64` algorithm on the serialized canonical
#' form.
#'
#' @param request A list or character string (passed through [sc_fingerprint()]).
#' @param version Cache version string (default `"v1"`).
#' @return A list with components:
#'   \describe{
#'     \item{key_hash}{xxhash64 of the full canonical request}
#'     \item{params_hash}{xxhash64 of params sub-list, or `NA_character_`}
#'     \item{tool_schema_hash}{xxhash64 of tools sub-list, or `NA_character_`}
#'   }
#' @examples
#' sc_key("What is 2+2?")
#' @importFrom digest digest
#' @export
sc_key <- function(request, version = "v1") {
  canonical <- sc_fingerprint(request, version = version)
  key_hash <- digest::digest(serialize(canonical, NULL), algo = "xxhash64")
  params_hash <- NA_character_
  if (!is.null(canonical$params) && length(canonical$params) > 0L) {
    params_hash <- digest::digest(
      serialize(canonical$params, NULL), algo = "xxhash64"
    )
  }
  tool_schema_hash <- NA_character_
  if (!is.null(canonical$tools) && length(canonical$tools) > 0L) {
    tool_schema_hash <- digest::digest(
      serialize(canonical$tools, NULL), algo = "xxhash64"
    )
  }
  list(
    key_hash = key_hash,
    params_hash = params_hash,
    tool_schema_hash = tool_schema_hash
  )
}
