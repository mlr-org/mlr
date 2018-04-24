#' @title Returns a list of mlr's options.
#'
#' @description
#' Gets the options for mlr.
#'
#' @return ([list]).
#' @export
#' @family configure
getMlrOptions = function() {
  mlr.options = .Options[stri_startswith_fixed(names(.Options), "mlr.")]
  names(mlr.options) = stri_sub(names(mlr.options), from = 5L)
  mlr.options[!stri_startswith_fixed(names(mlr.options), "debug.")]
}

setMlrOption = function(name, val) {
  name = sprintf("mlr.%s", name)
  do.call(options, setNames(list(val), name))
}

getMlrOption = function(name, default = NULL) {
  getOption(stri_paste("mlr.", name), default)
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
