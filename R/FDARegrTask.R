#' @title Functional data regression task.
#'
#' @description
#' Create a functional data regression task. This means that some features
#' in the task will be so-called functional covariates / functional featutes,
#' measured on a grid or time scale.
#'
#' @inheritParams Task
#' @template arg_fdatask_pars
#' @return See [\code{\link{FDARegrTask}}].
#' @export
#' @aliases FDARegrTask
makeFDARegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  task = makeRegrTask(id, data, target, weights, blocking, fixup.data, check.data)
  # arg checks for fd.features adn fd.grids are done in next call
  makeFDATask(task, "fdaregr", fd.features, fd.grids, "FDARegrTask", "FDARegrTaskDesc")
}

#FIXME: to make subsetTask work, otherwise, subsetTask will generate a regression task
makeTaskDesc.FDARegrTask = function(task, id, target, td) {
  badtd = makeTaskDesc.RegrTask(task = task , id = id, target = target)
  badtd$type = "fdaregr"
  badtd$fd.features = td$fd.features
  badtd$fd.grids = td$fd.grids
  addClasses(badtd, "FDARegrTaskDesc")
}
