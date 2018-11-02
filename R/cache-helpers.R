#' Get or delete mlr cache directory
#'
#' @description Helper functions to deal with mlr caching.
#' @name cache-helpers
#' @rdname cache-helpers
#' @details
#' `get_cache_dir()` returns the default mlr cache directory \cr\cr
#'  `delete_cache()` clears the default mlr cache directory. Custom cache
#'  directories must be deleted by hand.
NULL

#' @rdname cache-helpers
#' @export
get_cache_dir = function() {
  rappdirs::user_cache_dir("mlr", "mlr-org")
}

#' @rdname cache-helpers
#' @export
delete_cache = function() {
  fs::dir_delete(rappdirs::user_cache_dir("mlr", "mlr-org"))
  catf("Successfully cleared directory '%s'.", rappdirs::user_cache_dir("mlr", "mlr-org"))
}
