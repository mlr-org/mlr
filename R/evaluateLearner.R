#' @title Evaluates expressions within a learner according to the task.
#'
#' @description Updates the learner by evaluating its expressions based on a specific task.
#' @template arg_learner
#' @template arg_task
#' @return [\code{\link{Learner}}].
#' @example 
#' ## one can evaluate hyperparameters
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p))
#' lrn2 = evaluateLearner(lrn = lrn1, task = task)
#' 
#' lrn1$par.vals$minsplit
#' lrn2$par.vals$minsplit
#' 
#' ## alternatively, one can evaluate entire parameter sets
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.randomForest")
#' lrn2 = evaluateLearner(lrn = lrn1, task = task)
#' 
#' ## focus on the parameters 'mtry', 'classwt' and 'cutoff'
#' lrn1$par.set
#' lrn2$par.set
#' @export
evaluateLearner = function(lrn, task) {
  dict = makeTaskDictionary(task)
  if (!is.null(dict)) {
    if (ParamHelpers::hasExpression(lrn$par.set)) {
      ParamHelpers::checkParamSet(lrn$par.set, dict = dict)
      lrn$par.set = ParamHelpers::evaluateParamSet(par.set = lrn$par.set, dict = dict)
    }
    if (length(lrn$par.vals) > 0 && any(vlapply(lrn$par.vals, is.expression)))
      lrn$par.vals = lapply(lrn$par.vals, function(expr) eval(expr, envir = dict))
  }
  return(lrn)
}
