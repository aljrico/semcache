#' Open or Create a Semantic Cache
#'
#' Opens an existing cache or creates a new one at the given path. The cache
#' supports exact (hash-based) lookups and optional semantic (embedding-based)
#' lookups via **usearchlite**.
#'
#' @param path Directory for the cache. Created if it does not exist.
#' @param semantic One of `"auto"`, `"off"`, or `"on"`.
#'   - `"auto"`: enable semantic mode if **usearchlite** is installed and
#'     dimensionality can be determined.
#'   - `"off"`: exact-only mode.
#'   - `"on"`: require **usearchlite**; error if not available.
#' @param storage One of `"auto"`, `"rds"`, or `"toon"`.
#'   - `"auto"`: use RDS (default).
#'   - `"rds"`: store each entry as an RDS file.
#'   - `"toon"`: store each entry as a TOON file (requires **toonlite**).
#' @param dim Integer embedding dimension (required for new semantic caches;
#'   auto-discovered from existing config).
#' @param metric Distance metric: `"cosine"` (default) or `"l2"`.
#' @param version Cache version string (default `"v1"`).
#' @param strict Logical. If `TRUE` (default), semantic reuse requires matching
#'   model, tool_schema_hash, version, and temperature bucket.
#' @return A `semcache` object (environment-based S3 class).
#' @examples
#' \donttest{
#' cache <- sc_open(tempfile("semcache_"))
#' print(cache)
#' }
#' @export
sc_open <- function(path,
                    semantic = c("auto", "off", "on"),
                    storage = c("auto", "rds", "toon"),
                    dim = NULL,
                    metric = c("cosine", "l2"),
                    version = "v1",
                    strict = TRUE) {
  semantic <- match.arg(semantic)
  storage <- match.arg(storage)
  metric <- match.arg(metric)
  path <- normalizePath(path, mustWork = FALSE)

  # Create directory structure
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  blobs_dir <- file.path(path, "blobs")
  if (!dir.exists(blobs_dir)) dir.create(blobs_dir)

  config_path <- file.path(path, "config.rds")
  meta_path <- file.path(path, "meta.rds")

  # Load or create config
 if (file.exists(config_path)) {
    config <- readRDS(config_path)
    # Honour stored dim if not provided
    if (is.null(dim) && !is.na(config$dim)) dim <- config$dim
  } else {
    # Resolve storage backend
    storage_backend <- if (storage == "auto") {
      "rds"
    } else {
      storage
    }
    if (storage_backend == "toon" &&
        !requireNamespace("toonlite", quietly = TRUE)) {
      stop("toonlite is required for the toon storage backend")
    }
    config <- list(
      version = version,
      created_at = Sys.time(),
      strict = strict,
      storage_backend = storage_backend,
      semantic_enabled = FALSE,
      dim = NA_integer_,
      metric = metric,
      schema = 1L
    )
  }

  # Resolve semantic mode
  has_usearch <- requireNamespace("usearchlite", quietly = TRUE)
  sem_enabled <- switch(semantic,
    "off" = FALSE,
    "on" = {
      if (!has_usearch) stop("usearchlite is required for semantic mode")
      TRUE
    },
    "auto" = has_usearch && (!is.null(dim) || !is.na(config$dim))
  )
  if (sem_enabled && is.null(dim) && is.na(config$dim)) {
    stop("Embedding dimension (dim) is required for semantic mode")
  }
  config$semantic_enabled <- sem_enabled
  if (sem_enabled && !is.null(dim)) config$dim <- as.integer(dim)

  # Load or create meta
  meta <- if (file.exists(meta_path)) readRDS(meta_path) else empty_meta()

  # Build key2id environment
  key2id <- new.env(hash = TRUE, parent = emptyenv())
  if (nrow(meta) > 0L) {
    for (i in seq_len(nrow(meta))) {
      key2id[[meta$key_hash[i]]] <- meta$id[i]
    }
  }

  # Semantic index
  index <- NULL
  if (sem_enabled && has_usearch) {
    index_path <- file.path(path, "index.usearch")
    index <- usearchlite::index_new(
      dim = config$dim, path = index_path
    )
  }

  # Save config
  saveRDS(config, config_path)

  # Build cache object
  cache <- new.env(parent = emptyenv())
  cache$path <- path
  cache$config <- config
  cache$meta <- meta
  cache$key2id <- key2id
  cache$semantic <- sem_enabled
  cache$index <- index
  cache$hits <- 0L
  cache$misses <- 0L
  class(cache) <- "semcache"
  cache
}

#' Print a semcache Object
#'
#' Displays a one-line summary of a cache including entry count, mode,
#' storage backend, and path.
#'
#' @param x A `semcache` object.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.semcache <- function(x, ...) {
  n <- nrow(x$meta)
  sem <- if (x$semantic) {
    paste0("semantic (dim=", x$config$dim, ")")
  } else {
    "exact-only"
  }
  cat(sprintf(
    "<semcache> %d entries | %s | %s backend | %s\n",
    n, sem, x$config$storage_backend, x$path
  ))
  invisible(x)
}
