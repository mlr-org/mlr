#' @title Functional data classification task.
#'
#' @description
#' Create a functional data classification task. This means that some features 
#' in the task will be so-called functional covariates / functional features, 
#' measured on a grid or time scale. Different covariates might come from
#' different sensors for example.
#'
#' @inheritParams Task
#' @template arg_fdatask_pars
#' @return [\code{\link{FDAClassifTask}}].
#' @export
#' @examples
#' dat = data.frame(matrix(rnorm(20), nrow = 2))
#' dat$target = as.factor(c(0,1))
#' # X1 to X5 is functional covariate 1 and X6 to X10 functional covariate 2
#' # grd specifies the time points the curves were sampled at.
#' grd = list(fd_1 = 1:5, fd_2 = 1:5)
#' # One row per Observation
#' tsk = makeFDAClassifTask(data = dat, fd.features = list(fd_1 = 1:5, fd_2 = 6:10),
#'   target = "target", fd.grid = grd, positive = "1")
#' @aliases FDAClassifTask
makeFDAClassifTask = function(id = deparse(substitute(data)), data, target,
  weights = NULL, blocking = NULL, positive = NA_character_, fixup.data = "warn",
  check.data = TRUE, fd.features = NULL, fd.grids = NULL) {

  task = makeClassifTask(id, data, target, weights, blocking, positive, fixup.data, check.data)
  # arg checks for fd.features and fd.grids are done in next call
  convertTaskToFDATask(task, "fdaclassif", fd.features, fd.grids, "FDAClassifTask", "FDAClassifTaskDesc")
}

# td is the old task description, the function returns a new FDAClassifTask description
makeFDAClassifTaskDesc = function(id, data, target, positive, fd.features, fd.grids, weights, blocking) {
  new.td = makeClassifTaskDesc(id, data, target, weights, blocking, positive)
  new.td$type = "fdaclassif"
  # feat.remain = getTaskFeatureNames(task)
  feat.remain = setdiff(names(data), target)
  # Create new fields called fd.features and fd.grids for functional data
  updated.desc = updateFDATaskDesc(fd.features, fd.grids, feat.remain)
  new.td$fd.features = updated.desc$fd.features
  new.td$fd.grids = updated.desc$fd.grids
  addClasses(new.td, "FDAClassifTaskDesc")
}

