#' Prune Cache Entries
#'
#' Removes expired entries and optionally prunes to size/count limits.
#'
#' @param cache A `semcache` object from [sc_open()].
#' @param older_than A `POSIXct` time; entries created before this are pruned.
#' @param max_entries Maximum number of entries to keep (oldest removed first).
#' @param max_bytes Maximum total blob size in bytes (oldest removed first).
#' @param dry_run If `TRUE`, return what would be deleted without deleting.
#' @return A list with `n_removed` and `ids_removed` (invisibly).
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"), semantic = "off")
#' sc_put(cache, "q1", "a1")
#' sc_put(cache, "q2", "a2")
#' sc_prune(cache, max_entries = 1)
#' }
#' @export
sc_prune <- function(cache,
                     older_than = NULL,
                     max_entries = NULL,
                     max_bytes = NULL,
                     dry_run = FALSE) {
  stopifnot(inherits(cache, "semcache"))
  meta <- cache$meta
  if (nrow(meta) == 0L) {
    return(invisible(list(n_removed = 0L, ids_removed = integer(0L))))
  }

  remove_ids <- integer(0L)

  # 1) Remove expired entries
  expired <- vapply(meta$expires_at, is_expired, logical(1L))
  remove_ids <- c(remove_ids, meta$id[expired])

  # 2) Remove entries older than a cutoff
 if (!is.null(older_than)) {
    old <- meta$created_at < older_than
    remove_ids <- c(remove_ids, meta$id[old])
  }

  remove_ids <- unique(remove_ids)

  # 3) After removing the above, check max_entries
  remaining <- meta[!meta$id %in% remove_ids, , drop = FALSE]
  if (!is.null(max_entries) && nrow(remaining) > max_entries) {
    # Remove oldest first
    remaining <- remaining[order(remaining$created_at), , drop = FALSE]
    excess <- nrow(remaining) - max_entries
    remove_ids <- c(remove_ids, remaining$id[seq_len(excess)])
  }

  # 4) Check max_bytes (after all other removals)
  if (!is.null(max_bytes)) {
    remaining2 <- meta[!meta$id %in% remove_ids, , drop = FALSE]
    remaining2 <- remaining2[order(remaining2$created_at), , drop = FALSE]
    total <- 0
    keep_ids <- integer(0L)
    # Keep newest first by reversing
    for (i in rev(seq_len(nrow(remaining2)))) {
      blob_path <- file.path(
        cache$path, "blobs", remaining2$blob_ref[i]
      )
      sz <- if (file.exists(blob_path)) file.info(blob_path)$size else 0
      if (total + sz <= max_bytes) {
        total <- total + sz
        keep_ids <- c(keep_ids, remaining2$id[i])
      } else {
        remove_ids <- c(remove_ids, remaining2$id[i])
      }
    }
  }

  remove_ids <- unique(remove_ids)

  if (dry_run) {
    return(list(n_removed = length(remove_ids), ids_removed = remove_ids))
  }

  if (length(remove_ids) == 0L) {
    return(invisible(list(n_removed = 0L, ids_removed = integer(0L))))
  }

  # Delete blobs
  rows_to_rm <- meta[meta$id %in% remove_ids, , drop = FALSE]
  for (i in seq_len(nrow(rows_to_rm))) {
    delete_blob(cache, rows_to_rm$blob_ref[i])
  }

  # Update meta
  cache$meta <- meta[!meta$id %in% remove_ids, , drop = FALSE]

  # Update key2id
  for (kh in rows_to_rm$key_hash) {
    if (exists(kh, envir = cache$key2id, inherits = FALSE)) {
      rm(list = kh, envir = cache$key2id)
    }
  }

  # Rebuild semantic index if needed
  if (cache$semantic && !is.null(cache$index) && nrow(cache$meta) > 0L) {
    # Rebuild index from remaining entries that have embeddings
    index_path <- file.path(cache$path, "index.usearch")
    if (file.exists(index_path)) unlink(index_path)
    cache$index <- usearchlite::index_new(
      dim = cache$config$dim, path = index_path
    )
    kept_with_emb <- cache$meta[cache$meta$has_embedding, , drop = FALSE]
    for (i in seq_len(nrow(kept_with_emb))) {
      entry <- read_blob(
        cache, kept_with_emb$blob_ref[i], kept_with_emb$storage[i]
      )
      if (!is.null(entry$embedding)) {
        usearchlite::index_add(
          cache$index,
          id = as.integer(kept_with_emb$id[i]),
          vector = as.numeric(entry$embedding)
        )
      }
    }
  } else if (cache$semantic && !is.null(cache$index) &&
             nrow(cache$meta) == 0L) {
    index_path <- file.path(cache$path, "index.usearch")
    if (file.exists(index_path)) unlink(index_path)
    cache$index <- usearchlite::index_new(
      dim = cache$config$dim, path = index_path
    )
  }

  # Persist
  saveRDS(cache$meta, file.path(cache$path, "meta.rds"))
  saveRDS(cache$config, file.path(cache$path, "config.rds"))

  invisible(list(n_removed = length(remove_ids), ids_removed = remove_ids))
}
