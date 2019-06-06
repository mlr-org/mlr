#' @title Remove hyperparameters settings of a learner.
#'
#' @description
#' Remove settings (previously set through mlr) for some parameters.
#' Which means that the default behavior for that param will now be used.
#'
#' @template arg_learner
#' @param ids ([character])\cr
#'   Parameter names to remove settings for.
#'    Default is `character(0L)`.
#' @template ret_learner
#' @export
#' @family learner
removeHyperPars = function(learner, ids = character(0L)) {
  assertClass(learner, classes = "Learner")
  assertCharacter(ids, any.missing = FALSE)
  d = setdiff(ids, names(getHyperPars(learner)))
  if (length(d) > 0L) {
    stopf("Trying to remove param settings which were not set before: %s", collapse(d))
  }
  UseMethod("removeHyperPars")
}

#' @export
removeHyperPars.Learner = function(learner, ids = character(0L)) {
  learner$par.vals[ids] = NULL
  return(learner)
}
