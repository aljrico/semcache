# Internal utility functions for semcache

# Compact JSON serialization for simple lists.
# Uses jsonlite if available, otherwise a minimal fallback.
to_json <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  } else {
    # Minimal fallback for simple named lists of scalars
    if (is.null(x) || length(x) == 0L) return("{}")
    vals <- vapply(x, function(v) {
      if (is.null(v)) return("null")
      if (is.na(v)) return("null")
      if (is.character(v)) return(paste0('"', v, '"'))
      as.character(v)
    }, character(1L))
    paste0("{", paste(paste0('"', names(vals), '":', vals), collapse = ","), "}")
  }
}

from_json <- function(x) {
  if (is.na(x) || !nzchar(x) || x == "{}") return(list())
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::fromJSON(x, simplifyVector = FALSE)
  } else {
    # Cannot parse arbitrary JSON without jsonlite; return raw string
    warning("jsonlite not available; returning raw JSON string")
    x
  }
}

# Recursively sort named lists by name for canonical representation.
# Messages (lists of lists with role/content) are preserved in order.
canonicalize_recursive <- function(x, preserve_order = FALSE) {
  if (!is.list(x)) return(x)
  # Remove NULL elements
  x <- x[!vapply(x, is.null, logical(1L))]
  if (length(x) == 0L) return(x)
  nms <- names(x)
  if (!is.null(nms) && !preserve_order) {
    x <- x[order(nms)]
  }
  for (i in seq_along(x)) {
    if (is.list(x[[i]])) {
      # Messages list: list of list(role, content, ...) - preserve order
      is_msg_list <- !is.null(names(x)) && identical(names(x)[i], "messages")
      x[[i]] <- canonicalize_recursive(x[[i]], preserve_order = is_msg_list)
    }
  }
  x
}

# Bucket temperature to nearest 0.2 for strict mode matching
bucket_temperature <- function(temp) {
  if (is.null(temp) || is.na(temp)) return(NA_real_)
  round(temp / 0.2) * 0.2
}

# Generate zero-padded blob reference
blob_ref <- function(id, ext = "rds") {
  sprintf("%06d.%s", id, ext)
}

# Get next available ID from meta
next_id <- function(meta) {
  if (is.null(meta) || nrow(meta) == 0L) 1L else max(meta$id) + 1L
}

# Create an empty meta data.frame with the right columns
empty_meta <- function() {
  data.frame(
    id = integer(0L),
    key_hash = character(0L),
    created_at = as.POSIXct(character(0L)),
    expires_at = as.POSIXct(character(0L)),
    ttl_seconds = numeric(0L),
    model = character(0L),
    temperature = numeric(0L),
    params_hash = character(0L),
    tool_schema_hash = character(0L),
    version = character(0L),
    tags_json = character(0L),
    cost_json = character(0L),
    timing_ms = numeric(0L),
    has_embedding = logical(0L),
    embed_dim = integer(0L),
    storage = character(0L),
    blob_ref = character(0L),
    stringsAsFactors = FALSE
  )
}

# Check if an entry is expired
is_expired <- function(expires_at) {
  if (is.na(expires_at)) return(FALSE)
  expires_at < Sys.time()
}

# Compute disk size of a directory
dir_size <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0L) return(0)
  sum(file.info(files)$size, na.rm = TRUE)
}

# Read a blob from disk
read_blob <- function(cache, blob_ref_val, storage_type) {
  if (storage_type == "toon" &&
      requireNamespace("toonlite", quietly = TRUE)) {
    toonlite::read_toon(file.path(cache$path, "blobs", blob_ref_val))
  } else {
    readRDS(file.path(cache$path, "blobs", blob_ref_val))
  }
}

# Write a blob to disk
write_blob <- function(cache, entry, blob_ref_val, storage_type) {
  blob_path <- file.path(cache$path, "blobs", blob_ref_val)
  if (storage_type == "toon" &&
      requireNamespace("toonlite", quietly = TRUE)) {
    toonlite::write_toon(entry, blob_path, strict = FALSE)
  } else {
    saveRDS(entry, blob_path)
  }
}

# Delete a blob from disk
delete_blob <- function(cache, blob_ref_val) {
  blob_path <- file.path(cache$path, "blobs", blob_ref_val)
  if (file.exists(blob_path)) unlink(blob_path)
}
