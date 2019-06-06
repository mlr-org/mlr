#' @title Get the class weight parameter of a learner.
#'
#' @description
#' Gets the class weight parameter of a learner.
#'
#' @template arg_learner
#' @param lrn.id ([character])\cr
#'   Only used for `BaseEnsembles`. It is possible that multiple learners in a base
#'   ensemble have a class weight param. Specify the learner from which the class weight should
#'   be extracted.
#' @return [numeric] [LearnerParam]:
#'   A numeric parameter object, containing the class weight parameter of the given learner.
#' @family learner
#' @export
getClassWeightParam = function(learner, lrn.id = NULL) {
  UseMethod("getClassWeightParam")
}

#' @export
getClassWeightParam.character = function(learner, ...) {
  learner = checkLearner(learner, "classif", props = "class.weights")
  getClassWeightParam(learner, ...)
}


#' @export
getClassWeightParam.Learner = function(learner, ...) {
  learner = checkLearner(learner, "classif", props = "class.weights")
  weight.param.name = learner$class.weights.param
  learner$par.set$pars[[weight.param.name]]
}
