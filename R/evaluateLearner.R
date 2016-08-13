#' @title Evaluates expressions within a learner or parameter set according to the task.
#'
#' @description Updates learners and/or parameter sets by evaluating their expressions
#' based on a specific task. An overview of the possible expressions can be found in the details.
#' @template arg_learner
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set of (hyper)parameters and their constraints.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @template arg_task
#' @return [\code{\link{Learner}} | \code{\link[ParamHelpers]{ParamSet}}].
#' @name evaluateLearner
#' @rdname evaluateLearner
#' @details The expressions can be based on any information provided by the task. For convenience,
#' the most often used keys are available directly
#' \itemize{
#'   \item{\code{task}:} the task itself, allowing to access any of its elements
#'   \item{\code{p}:} the number of features in the task
#'   \item{\code{n}:} the number of observations in the task
#'   \item{\code{type}:} the task type, i.e. "classif", "regr", "surv", "cluster", "costcens" or "multilabel"
#'   \item{\code{k}:} the number of classes of the target variable (only available for classification tasks)
#' }
#' However, if one wants to access any other parts of the \code{task}, one can do so. For instance, one could
#' access the "blocking" via \code{task$task.desc$has.blocking}.
#' @examples
#' ## (1) evaluation of a learner's hyperparameters
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p),
#'   minbucket = expression(3L + 4L * task$task.desc$has.blocking))
#' lrn2 = evaluateLearner(learner = lrn1, task = task)
#' 
#' lrn1$par.vals
#' lrn2$par.vals
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
#'   makeNumericParam("C", lower = expression(k), upper = expression(n), trafo = function(x) 2^x),
#'   makeDiscreteParam("sigma", values = expression(list(k, p)))
#' )
#' ps2 = evaluateParset(par.set = ps1, task = task)
#' @export
evaluateLearner = function(learner, task) {
  dict = makeTaskDictionary(task = task)
  learner$par.set = evaluateParset(learner$par.set, task = task)
  if (length(learner$par.vals) > 0 && any(vlapply(learner$par.vals, is.expression)))
    learner$par.vals = lapply(learner$par.vals, function(expr) eval(expr, envir = dict))
  return(learner)
}

#' @rdname evaluateLearner
#' @export
evaluateParset = function(par.set, task) {
  dict = makeTaskDictionary(task = task)
  if (hasExpression(par = par.set)) {
    checkParamSet(par.set = par.set, dict = dict)
    par.set = evaluateParamSet(par.set = par.set, dict = dict)
    ## assure that the value names are also shown if the values list was unnamed
    par.set$pars = lapply(par.set$pars, function(x) {
      if (is.null(x$values) || !is.null(names(x$values)))
        return(x)
      names(x$values) = unlist(lapply(x$values, function(vals) vals))
      return(x)
    })
  }
  return(par.set)
}
