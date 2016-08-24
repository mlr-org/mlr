#' @title Get the type of the learner.
#'
#' @description Get the type of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerType = function(learner) {
  learner = checkLearner(learner)
  return(learner$type)
}

#' @title Get the ID of the learner.
#'
#' @description Get the ID of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerId = function(learner) {
  learner = checkLearner(learner)
  return(learner$id)
}

#' @title Get the predict type of the learner.
#'
#' @description Get the predict type of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerPredictType = function(learner) {
  learner = checkLearner(learner)
  return(learner$predict.type)
}

#' @title Get the required R packages of the learner.
#'
#' @description Get the R packages the learner requires.
#' @template arg_learner
#' @return [\code{character}].
#' @export
#' @family learner
getLearnerPackages = function(learner) {
  learner = checkLearner(learner)
  return(learner$package)
}


#' @title Get the parameter set of the learner.
#'
#' @description
#' Alias for \code{\link{getParamSet}}.
#'
#' @template arg_learner
#' @template ret_ps
#' @export
#' @family learner
getLearnerParamSet = function(learner) {
  getParamSet(learner)
}


#' @title Get the parameter values of the learner.
#'
#' @description
#' Alias for \code{\link{getHyperPars}}.
#'
#' @template arg_learner
#' @inheritParams getHyperPars
#' @return [\code{list}]. A named list of values.
#' @export
#' @family learner
getLearnerParVals = function(learner, for.fun = c("train", "predict", "both")) {
  learner = checkLearner(learner)
  getHyperPars(learner, for.fun)
}

#' @title Set the ID of a learner object.
#'
#' @description Set the ID of the learner.
#' @template arg_learner
#' @param id [\code{character(1)}]\cr
#'    New ID for learner.
#' @template ret_learner
#' @export
#' @family learner
setLearnerId = function(learner, id) {
  learner = checkLearner(learner)
  assertString(id)
  learner$id = id
  return(learner)
}



