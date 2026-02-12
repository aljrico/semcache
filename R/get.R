#' Retrieve an Entry from the Cache
#'
#' Looks up a cached response by exact hash match or semantic similarity.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @param request A list or character string describing the LLM request.
#' @param mode One of `"exact"`, `"semantic"`, or `"both"` (default).
#' @param embedding Numeric vector for semantic search. Required if
#'   `mode` includes `"semantic"`.
#' @param k Maximum number of semantic candidates to return.
#' @param prefilter_k Number of ANN candidates to retrieve before filtering.
#' @param threshold Minimum cosine similarity for a semantic hit (default
#'   0.92).
#' @param allow_stale If `FALSE` (default), expired entries are skipped.
#' @param strict If `TRUE`, semantic reuse requires matching model,
#'   tool_schema_hash, version, and temperature bucket. Defaults to the cache
#'   setting.
#' @return A list with components:
#'   \describe{
#'     \item{hit}{Logical: was a cache entry found?}
#'     \item{mode}{`"exact"`, `"semantic"`, or `NA` if miss}
#'     \item{id}{Entry ID (or `NA`)}
#'     \item{score}{Similarity score for semantic hits (`NA` for exact)}
#'     \item{entry}{The cached entry list, or `NULL`}
#'     \item{candidates}{Data frame of semantic candidates (or `NULL`)}
#'   }
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' sc_put(cache, "What is 2+2?", "4")
#' result <- sc_get(cache, "What is 2+2?", mode = "exact")
#' result$hit
#' }
#' @export
sc_get <- function(cache,
                   request,
                   mode = c("both", "exact", "semantic"),
                   embedding = NULL,
                   k = 1L,
                   prefilter_k = 50L,
                   threshold = 0.92,
                   allow_stale = FALSE,
                   strict = cache$config$strict) {
  stopifnot(inherits(cache, "semcache"))
  mode <- match.arg(mode)
  keys <- sc_key(request, version = cache$config$version)
  canonical <- sc_fingerprint(request, version = cache$config$version)

  miss <- list(
    hit = FALSE, mode = NA_character_, id = NA_integer_,
    score = NA_real_, entry = NULL, candidates = NULL
  )

  # --- Exact lookup ---
  if (mode %in% c("exact", "both")) {
    cached_id <- cache$key2id[[keys$key_hash]]
    if (!is.null(cached_id)) {
      row <- cache$meta[cache$meta$id == cached_id, , drop = FALSE]
      if (nrow(row) == 1L) {
        if (allow_stale || !is_expired(row$expires_at[1L])) {
          entry <- read_blob(cache, row$blob_ref[1L], row$storage[1L])
          cache$hits <- cache$hits + 1L
          return(list(
            hit = TRUE, mode = "exact", id = cached_id,
            score = NA_real_, entry = entry, candidates = NULL
          ))
        }
      }
    }
  }

  # --- Semantic lookup ---
  if (mode %in% c("semantic", "both") && cache$semantic) {
    if (is.null(embedding)) {
      if (mode == "semantic") {
        stop("embedding is required for semantic search")
      }
      # In 'both' mode, fall through to miss if no embedding
      cache$misses <- cache$misses + 1L
      return(miss)
    }
    stopifnot(
      is.numeric(embedding),
      length(embedding) == cache$config$dim
    )

    # Query ANN index
    n_indexed <- nrow(usearchlite::index_meta(cache$index))
    actual_k <- min(prefilter_k, n_indexed)
    if (actual_k == 0L) {
      cache$misses <- cache$misses + 1L
      return(miss)
    }
    res <- usearchlite::index_search(
      cache$index,
      query = as.numeric(embedding),
      k = as.integer(actual_k)
    )
    if (length(res$ids) == 0L) {
      cache$misses <- cache$misses + 1L
      return(miss)
    }

    # Convert cosine distance to similarity
    similarities <- 1 - res$distances

    # Build candidates data.frame for filtering
    cand_ids <- res$ids
    cand_rows <- cache$meta[match(cand_ids, cache$meta$id), , drop = FALSE]

    # Filter
    keep <- rep(TRUE, length(cand_ids))

    # Expiry filter
    if (!allow_stale) {
      keep <- keep & vapply(
        cand_rows$expires_at, function(ea) !is_expired(ea), logical(1L)
      )
    }

    # Version match
    keep <- keep & (cand_rows$version == cache$config$version)

    # Strict filters
    if (strict) {
      req_model <- if (is.null(canonical$model)) NA_character_ else canonical$model
      req_tsh <- keys$tool_schema_hash
      req_temp <- bucket_temperature(canonical$params$temperature)

      if (!is.na(req_model)) {
        keep <- keep & (!is.na(cand_rows$model) &
                         cand_rows$model == req_model)
      }
      if (!is.na(req_tsh)) {
        keep <- keep & (!is.na(cand_rows$tool_schema_hash) &
                         cand_rows$tool_schema_hash == req_tsh)
      }
      if (!is.na(req_temp)) {
        cand_temps <- vapply(
          cand_rows$temperature, bucket_temperature, numeric(1L)
        )
        keep <- keep & (!is.na(cand_temps) & cand_temps == req_temp)
      }
    }

    # Threshold filter
    keep <- keep & (similarities >= threshold)

    if (!any(keep)) {
      # Build candidates df for diagnostics
      candidates_df <- data.frame(
        id = cand_ids, score = similarities,
        created_at = cand_rows$created_at,
        model = cand_rows$model,
        stringsAsFactors = FALSE
      )
      cache$misses <- cache$misses + 1L
      return(list(
        hit = FALSE, mode = NA_character_, id = NA_integer_,
        score = NA_real_, entry = NULL, candidates = candidates_df
      ))
    }

    # Select best among kept
    kept_idx <- which(keep)
    best_i <- kept_idx[which.max(similarities[kept_idx])]
    best_id <- cand_ids[best_i]
    best_score <- similarities[best_i]
    best_row <- cand_rows[best_i, , drop = FALSE]
    entry <- read_blob(cache, best_row$blob_ref, best_row$storage)

    candidates_df <- data.frame(
      id = cand_ids[keep], score = similarities[keep],
      created_at = cand_rows$created_at[keep],
      model = cand_rows$model[keep],
      stringsAsFactors = FALSE
    )

    cache$hits <- cache$hits + 1L
    return(list(
      hit = TRUE, mode = "semantic", id = best_id,
      score = best_score, entry = entry,
      candidates = candidates_df[seq_len(min(k, nrow(candidates_df))), ,
                                  drop = FALSE]
    ))
  }

  cache$misses <- cache$misses + 1L
  miss
}
