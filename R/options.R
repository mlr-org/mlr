#' @title Returns a list of mlr's options
#'
#' @return [\code{list}].
#' @export
#' @family configure
getMlrOptions = function() {
  mlr.inds = substr(names(options()), start = 1L, stop = 4L) == "mlr."
  mlr.options = options()[mlr.inds]
  names(mlr.options) = substring(names(mlr.options), first = 5L)
  mlr.debug.inds = substr(names(mlr.options), start = 1L, stop = 6L) == "debug."
  mlr.options[!mlr.debug.inds]
}

setMlrOption = function(name, val) {
  name = sprintf("mlr.%s", name)
  do.call(options, setNames(list(val), name))
}

getMlrOption = function(name, default) {
  name = sprintf("mlr.%s", name)
  getOption(name, default)
}

exportMlrOptions = function() {

}
