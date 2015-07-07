#' @title Get the class weight parameter of a learner.
#'
#' @template arg_learner
#' @return [\code{list}]. A named list of values.
#' @family learner
#' @export
getClassWeightParam = function(learner) {
  assert("classweights" %in% learner$properties)
  assertCharacter(learner$class.weights.param)
  cwp = learner$par.set$pars[[learner$class.weights.param]]
  if (is.null(cwp)) {
    warningf("The class weight parameter is called '%s'.
      However, it is currently not defined within the learner's parameter set.",
      learner$class.weights.param)
  } else
    return(cwp)
}
