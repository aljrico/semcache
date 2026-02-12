#' Store an Entry in the Cache
#'
#' Stores a request/response pair. The request is canonicalized and hashed
#' for exact lookup. An optional embedding vector enables semantic retrieval.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @param request A list or character string describing the LLM request.
#' @param response The response object (character or list).
#' @param embedding Optional numeric vector for semantic indexing.
#' @param ttl Time-to-live in seconds (`Inf` for no expiration).
#' @param tags A named list of tags for the entry.
#' @param cost A list with `prompt_tokens`, `completion_tokens`,
#'   `total_tokens`.
#' @param timing_ms Response time in milliseconds.
#' @param created_at Timestamp (default [Sys.time()]).
#' @return The integer ID of the new entry (invisibly).
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' sc_put(cache, "What is 2+2?", "4")
#' }
#' @export
sc_put <- function(cache,
                   request,
                   response,
                   embedding = NULL,
                   ttl = Inf,
                   tags = list(),
                   cost = list(
                     prompt_tokens = NA, completion_tokens = NA,
                     total_tokens = NA
                   ),
                   timing_ms = NA_real_,
                   created_at = Sys.time()) {
  stopifnot(inherits(cache, "semcache"))
  canonical <- sc_fingerprint(request, version = cache$config$version)
  keys <- sc_key(request, version = cache$config$version)

  id <- next_id(cache$meta)

  # Determine storage extension
  ext <- if (cache$config$storage_backend == "toon" &&
             requireNamespace("toonlite", quietly = TRUE)) "toon" else "rds"
  ref <- blob_ref(id, ext)

  # Build entry
  entry <- list(
    id = id,
    request = canonical,
    response = response,
    embedding = embedding,
    meta = list(
      created_at = created_at,
      model = if (is.null(canonical$model)) NA_character_ else canonical$model,
      params = canonical$params,
      tags = tags,
      cost = cost,
      timing_ms = timing_ms
    )
  )

  # Write blob
  write_blob(cache, entry, ref, cache$config$storage_backend)

  # Compute expires_at
  expires_at <- if (is.infinite(ttl)) NA else created_at + ttl

  # Update meta
  has_emb <- !is.null(embedding)
  emb_dim <- if (has_emb) length(embedding) else NA_integer_
  new_row <- data.frame(
    id = id,
    key_hash = keys$key_hash,
    created_at = created_at,
    expires_at = if (is.na(expires_at)) as.POSIXct(NA) else expires_at,
    ttl_seconds = ttl,
    model = if (is.null(canonical$model)) NA_character_ else canonical$model,
    temperature = if (!is.null(canonical$params$temperature)) {
      as.numeric(canonical$params$temperature)
    } else {
      NA_real_
    },
    params_hash = keys$params_hash,
    tool_schema_hash = keys$tool_schema_hash,
    version = cache$config$version,
    tags_json = as.character(to_json(tags)),
    cost_json = as.character(to_json(cost)),
    timing_ms = timing_ms,
    has_embedding = has_emb,
    embed_dim = emb_dim,
    storage = cache$config$storage_backend,
    blob_ref = ref,
    stringsAsFactors = FALSE
  )
  cache$meta <- rbind(cache$meta, new_row)

  # Update key2id
  cache$key2id[[keys$key_hash]] <- id

  # Semantic index
  if (has_emb && cache$semantic && !is.null(cache$index)) {
    stopifnot(
      is.numeric(embedding),
      length(embedding) == cache$config$dim,
      all(is.finite(embedding))
    )
    usearchlite::index_add(
      cache$index, id = as.integer(id), vector = as.numeric(embedding)
    )
  }

  # Persist meta
  saveRDS(cache$meta, file.path(cache$path, "meta.rds"))
  saveRDS(cache$config, file.path(cache$path, "config.rds"))

  invisible(id)
}

