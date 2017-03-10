#' @title Get the class weight parameter of a learner.
#'
#' @description
#' Gets the class weight parameter of a learner.
#'
#' @template arg_learner
#' @return [\code{numeric \link{LearnerParam}}]:
#'   A numeric parameter object, containing the class weight parameter of the given learner.
#' @family learner
#' @export
getClassWeightParam = function(learner) {
  learner = checkLearner(learner, "classif")
  assertChoice("class.weights", getLearnerProperties(learner))
  UseMethod("getClassWeightParam")
}


#' @export
getClassWeightParam.Learner = function(learner) {
  learner = assertClass(learner, "Learner")
  weight.param.name = learner$class.weights.param
  learner$par.set$pars[[weight.param.name]]  
}