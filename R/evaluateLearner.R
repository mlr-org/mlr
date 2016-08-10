#' @title Evaluates expressions within a learner or parameter set according to the task.
#'
#' @description Updates learners and/or parameter sets by evaluating their expressions
#' based on a specific task.
#' @template arg_learner
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set of (hyper)parameters and their constraints.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @template arg_task
#' @template arg_dict
#' @return [\code{\link{Learner}} | \code{\link[ParamHelpers]{ParamSet}}].
#' @name evaluateLearner
#' @rdname evaluateLearner
#' @examples
#' ## (1) evaluation of a learner's hyperparameters
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p))
#' lrn2 = evaluateLearner(learner = lrn1, task = task)
#' 
#' lrn1$par.vals$minsplit
#' lrn2$par.vals$minsplit
#' 
#' ## (2) evaluation of a learner's entire parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.randomForest")
#' lrn2 = evaluateLearner(learner = lrn1, task = task)
#' 
#' ## focus on the parameters 'mtry', 'classwt' and 'cutoff'
#' lrn1$par.set
#' lrn2$par.set
#' 
#' ## (3) evaluation of a parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' ps1 = makeParamSet(
#'   makeNumericParam("C", lower = expression(a), upper = expression(b), trafo = function(x) 2^x),
#'   makeDiscreteParam("sigma", values = 2^c(-1, 1)),
#'   makeDiscreteParam("kernel", values = expression(list(e, f)))
#' )
#' ps2 = evaluateParset(par.set = ps1, task = task,
#'   dict = list(a = -2, b = 3, e = "rbfdot", f = "laplacedot"))
#' @export
evaluateLearner = function(learner, task, dict = NULL) {
  task.dict = makeTaskDictionary(task)
  dict = insert(task.dict, dict)
  if (!is.null(dict)) {
    learner$par.set = evaluateParset(learner$par.set, task = task, dict = dict)
    if (length(learner$par.vals) > 0 && any(vlapply(learner$par.vals, is.expression)))
      learner$par.vals = lapply(learner$par.vals, function(expr) eval(expr, envir = dict))
  }
  return(learner)
}

#' @rdname evaluateLearner
#' @export
evaluateParset = function(par.set, task, dict = NULL) {
  task.dict = makeTaskDictionary(task = task)
  dict = insert(task.dict, dict)
  if (!is.null(dict)) {
    if (ParamHelpers::hasExpression(par = par.set)) {
      ParamHelpers::checkParamSet(par.set = par.set, dict = dict)
      par.set = ParamHelpers::evaluateParamSet(par.set = par.set, dict = dict)
      ## assure that the value names are also shown if the values list was unnamed
      par.set$pars = lapply(par.set$pars, function(x) {
        if (is.null(x$values) || !is.null(names(x$values)))
          return(x)
        names(x$values) = unlist(lapply(x$values, function(vals) vals))
        return(x)
      })
    }
  }
  return(par.set)
}
