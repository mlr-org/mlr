#' @title Get the type of the learner.
#'
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerType = function(learner) {
  learner = checkLearner(learner)
  return(learner$type)
}

#' @title Get the id of the learner.
#'
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
#' @template arg_learner
#' @return [\code{character}].
#' @export
#' @family learner
getLearnerPackages = function(learner) {
  learner = checkLearner(learner)
  return(learner$package)
}


#' @title Get the param set of the learner.
#'
#' @description
#' Simply an alternative name for \code{\link{getParamSet}}.
#'
#' @template arg_learner
#' @template ret_ps
#' @export
#' @family learner
getLearnerParamSet = function(learner) {
  learner = checkLearner(learner)
  getParamSet(learner)
}


#' @title Get the param values of the learner.
#'
#' @description
#' Simply an alternative name for \code{\link{getHyperPars}}.
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

#' @title Get the id of the learner.
#'
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerId = function(learner) {
  learner = checkLearner(learner)
  return(learner$id)
}

#' @title Set the id of a learner object.
#'
#' @template arg_learner
#' @param id [\code{character(1)}]\cr
#'    New id for learner.
#' @template ret_learner
#' @export
#' @family learner
setLearnerId = function(learner, id) {
  learner = checkLearner(learner)
  assertString(id)
  learner$id = id
  return(learner)
}



