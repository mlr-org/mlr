#' Display all possible hyperparameter settings for a learner that mlr knows.
#'
#' Useful for a quick overview, also does not force you to create the learner.
#' @template arg_learner
#' @template ret_inv_null
#' @family learner
#' @export
showHyperPars = function (learner) {
  learner = checkLearner(learner)
  print(learner$par.set, trafo = FALSE, used = FALSE)
  invisible(NULL)
}
