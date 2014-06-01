#' Display all possible hyperparameter settings for a learner that mlr knows.
#'
#' Useful for a quick overview, also does not foce you to create the learner.
#' @template arg_lrncl
#' @template ret_inv_null
#' @export
showHyperPars = function (cl) {
  lrn = makeLearner(cl)
  print(lrn$par.set, trafo = FALSE, used = FALSE)
  invisible(NULL)
}

