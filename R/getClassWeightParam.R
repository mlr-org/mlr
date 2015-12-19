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
  learner = checkLearnerClassif(learner)
  assertChoice("class.weights", getLearnerProperties(learner))
  learner$par.set$pars[[learner$class.weights.param]]
}
