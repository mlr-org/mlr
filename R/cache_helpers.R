#' @title Get or delete mlr cache directory
#'
#' @description Helper functions to deal with mlr caching.
#' @name cache_helpers
#' @rdname cache_helpers
#' @details
#' `getCacheDir()` returns the default mlr cache directory \cr
#' `deleteCacheDir()` clears the default mlr cache directory. Custom cache
#'  directories must be deleted by hand.
NULL

#' @rdname cache_helpers
#' @export
getCacheDir = function() {
  rappdirs::user_cache_dir("mlr", "mlr-org")
}

#' @rdname cache_helpers
#' @export
deleteCacheDir = function() {
  unlink(rappdirs::user_cache_dir("mlr", "mlr-org"), recursive = TRUE)
  catf("Successfully cleared directory '%s'.", rappdirs::user_cache_dir("mlr", "mlr-org"))
}
