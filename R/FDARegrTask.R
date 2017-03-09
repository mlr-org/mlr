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
#' @examples
#' dat = data.frame(matrix(rnorm(20), nrow = 2))
#' dat$target = c(1,2)
#' # X1 to X5 is covariate 1 and X6 to X10 covariate 2
#' # grd specifies the time points the curves were sampled at.
#' grd = list(ch_1 = 1:5, ch_2 = 1:5)
#' # One row per Observation
#' tsk = makeFDARegrTask(data = dat, fd.features = list(ch_1 = 1:5, ch_2 = 6:10),
#'   target = "target", fd.grid = grd)
#' @aliases FDARegrTask
makeFDARegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL,
  blocking = NULL, fixup.data = "warn", check.data = TRUE, fd.features = NULL, fd.grids = NULL) {
  task = makeRegrTask(id, data, target, weights, blocking, fixup.data, check.data)
  # arg checks for fd.features adn fd.grids are done in next call
  convertTaskToFDATask(task, "fdaregr", fd.features, fd.grids, "FDARegrTask", "FDARegrTaskDesc")
}

# td is the old task description, the function return a new task description
makeTaskDesc.FDARegrTask = function(task, id, target, fd.features, fd.grids) {
  new.td = makeTaskDesc.RegrTask(task = task , id = id, target = target)
  new.td$type = "fdaregr"
  feat.remain = getTaskFeatureNames(task)
  # Create new fields called fd.features and fd.grids for functional data (the same is done in makeFDATask)
  # to make subset(FDATask, features = 1:10) work for example, we need to adapt the fd.features and fd.grids according to the subseted global feature index.
  new.td$fd.features = setNames(lapply(names(fd.features), function(fdn) {
      fd.features[[fdn]][fd.features[[fdn]] %in% feat.remain]
    }), names(fd.features))
  # since feat.remain is a character vector, we have to use fd.features[[fdn]] rather than fd.grids[[fdn]]
  new.td$fd.grids = setNames(lapply(names(fd.features), function(fdn) {
      fd.grids[[fdn]][fd.features[[fdn]] %in% feat.remain]
    }), names(fd.grids))
  addClasses(new.td, "FDARegrTaskDesc")
}
