#' @title Functional data regression task.
#'
#' @description
#' Create a functional data regression task. This means that some features
#' in the task will be so-called functional covariates / functional features,
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

# td is the old task description, the function return a new task description
makeTaskDesc.FDARegrTask = function(task, id, target, td) {
  badtd = makeTaskDesc.RegrTask(task = task , id = id, target = target)
  badtd$type = "fdaregr"
  feat.remain = getTaskFeatureNames(task)
  # Create new fields called fd.features and fd.grids for functional data (the same is done in makeFDATask)
  badtd$fd.features = setNames(lapply(names(td$fd.features), function(fdn) td$fd.features[[fdn]][td$fd.features[[fdn]] %in% feat.remain]), names(td$fd.features))
  # since feat.remain is a character vector, we have to use fd.features[[fdn]] rather than fd.grids[[fdn]] 
  badtd$fd.grids = setNames(lapply(names(td$fd.features), function(fdn) td$fd.grids[[fdn]][td$fd.features[[fdn]] %in% feat.remain]), names(td$fd.grids))
  addClasses(badtd, "FDARegrTaskDesc")
}
