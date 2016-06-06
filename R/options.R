#' @title Returns a list of mlr's options.
#'
#' @description
#' Gets the options for mlr.
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

# FIXME: the mechanism here is not perfect.
# we export the options to the slaves, then read and set them
exportMlrOptions = function(level) {
  .mlr.slave.options = getMlrOptions()
  parallelExport(".mlr.slave.options", level = level, master = FALSE, show.info = FALSE)
}

setSlaveOptions = function() {
  if (getOption("parallelMap.on.slave", FALSE)) {
    # for multicocre the options are not exported, we also dont need them due to forking....
    if (exists(".mlr.slave.options", envir = .GlobalEnv)) {
      opts = get(".mlr.slave.options", envir = .GlobalEnv)
      Map(setMlrOption, names(opts), opts)
    }
  }
}
