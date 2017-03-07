#' @title Functional data classification task.
#'
#' @description
#' Create a functional data classification task. This means that some features
#' in the task will be so-called functional covariates / functional features,
#' measured on a grid or time scale.
#'
#' @inheritParams Task
#' @template arg_fdatask_pars
#' @return [\code{\link{FDAClassifTask}}].
#' @export
#' @aliases FDAClassifTask
makeFDAClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn",
  check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  # arg checks for fd.features and fd.grids are done in next call
  makeFDATask(task, "fdaclassif", fd.features, fd.grids, "FDAClassifTask", "FDAClassifTaskDesc")
}

# td is the old task description, the function returns a new FDAClassifTask description
makeTaskDesc.FDAClassifTask = function(task, id, target, td) {
  badtd = makeTaskDesc.ClassifTask(task = task , id = id, target = target, positive = td$positive)
  badtd$type = "fdaclassif"

  feat.remain = getTaskFeatureNames(task)
  # Create new fields called fd.features and fd.grids for functional data (the same is done in makeFDATask)
  badtd$fd.features = setNames(lapply(names(td$fd.features), function(fdn) {
      td$fd.features[[fdn]][td$fd.features[[fdn]] %in% feat.remain]
    }), names(td$fd.grids))
  # since feat.remain is a character vector with variable names, we use td$fd.features[[fdn]] for indexing
  badtd$fd.grids = setNames(lapply(names(td$fd.features), function(fdn) {
      td$fd.grids[[fdn]][td$fd.features[[fdn]] %in% feat.remain]
    }), names(td$fd.grids))
  addClasses(badtd, "FDAClassifTaskDesc")
}
