#' @title Get the type of the learner.
#'
#' @description Get the type of the learner.
#' @template arg_learner
#' @return (`character(1)`).
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
#' @return (`character(1)`).
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
#' @return (`character(1)`).
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
#' @return ([character]).
#' @export
#' @family learner
getLearnerPackages = function(learner) {
  learner = checkLearner(learner)
  return(learner$package)
}

#' @title Get the note for the learner.
#'
#' @description Get the note for the learner.
#' @template arg_learner
#' @return ([character]).
#' @export
#' @family learner
getLearnerNote = function(learner) {
  learner = checkLearner(learner)
  messagef(learner$note)
}

#' @title Get the parameter set of the learner.
#'
#' @description
#' Alias for [getParamSet].
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
#' Alias for [getHyperPars].
#'
#' @template arg_learner
#' @inheritParams getHyperPars
#' @return ([list]). A named list of values.
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
#' @param id (`character(1)`)\cr
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

#' @title Get the short name of the learner.
#'
#' @description For an ordinary learner simply its short name is returned.
#'   For wrapped learners, the wrapper id is successively attached to the short
#'   name of the base learner. E.g: \dQuote{rf.bagged.imputed}
#' @template arg_learner
#' @return (`character(1)`).
#' @export
#' @family learner
getLearnerShortName = function(learner) {
  learner = checkLearner(learner)
  learner.short.name = learner$short.name

  if (is.null(learner.short.name)) {
    learner.id.split = unlist(strsplit(getLearnerId(learner), "[.]"))
    wrapper.ids = learner.id.split[3:length(learner.id.split)]
    base.learner.path = c(rep("next.learner", length(wrapper.ids)),
      "short.name")
    base.learner.short.name = extractSubList(list(learner),
      base.learner.path)
    learner.short.name = paste(c(base.learner.short.name, wrapper.ids),
      collapse = ".")
  }

  return(learner.short.name)
}
