#' @title Evaluates expressions within a learner or parameter set according to the task.
#'
#' @description
#' A \code{\link{Learner}} or \code{\link[ParamHelpers]{ParamSet}} can contain an unevaluated \code{\link[base]{expression}}
#' as value for a hyperparameter.
#' E.g., these expressions are used if the default value dependents on the task size or an upper limit for a parameter
#' is given by the number of features in a task.
#' The provided functions evaluate such expressions in an environment (dictionary) which holds the following information:
#' \itemize{
#'   \item{\code{task}:} the task itself, allowing to access any of its elements.
#'   \item{\code{p}:} the number of features in the task
#'   \item{\code{n}:} the number of observations in the task
#'   \item{\code{type}:} the task type, i.e. "classif", "regr", "surv", "cluster", "costcens" or "multilabel"
#'   \item{\code{k}:} the number of classes of the target variable (only available for classification tasks)
#' }
#' Usually the evaluation of the expression is performed automatically, e.g. in \code{\link{train}} or
#' \code{\link{tuneParams}}.
#' Therefore calling \code{evaluateParamSet} or \code{evaluateLearner} manually should not be necessary.
#'
#' @template arg_learner
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set of (hyper)parameters and their constraints.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @template arg_task
#' @return [\code{\link{Learner}} | \code{\link[ParamHelpers]{ParamSet}}].
#' @name evaluateLearner
#' @rdname evaluateLearner
#' @export
#' @examples
#' ## (1) evaluation of a learner's hyperparameters
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p),
#'   minbucket = expression(3L + 4L * task$task.desc$has.blocking))
#' lrn2 = evaluateLearner(learner = lrn1, task = task)
#'
#' getHyperPars(lrn1)
#' getHyperPars(lrn2)
#'
#' ## (2) evaluation of a learner's entire parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn1 = makeLearner("classif.randomForest")
#' lrn2 = evaluateLearner(learner = lrn1, task = task)
#'
#' ## Note the values for parameters 'mtry', 'classwt' and 'cutoff'
#' getParamSet(lrn1)
#' getParamSet(lrn2)
#'
#' ## (3) evaluation of a parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' ps1 = makeParamSet(
#'   makeNumericParam("C", lower = expression(k), upper = expression(n), trafo = function(x) 2^x),
#'   makeDiscreteParam("sigma", values = expression(list(k, p)))
#' )
#' evaluateParset(par.set = ps1, task = task)
evaluateLearner = function(learner, task) {
  dict = makeTaskDictionary(task = task)
  learner$par.set = evaluateParset(learner$par.set, task = task)
  if (any(vlapply(learner$par.vals, is.expression)))
    learner$par.vals = lapply(learner$par.vals, function(expr) eval(expr, envir = dict))
  return(learner)
}

#' @rdname evaluateLearner
#' @export
evaluateParset = function(par.set, task) {
  if (hasExpression(par = par.set)) {
    dict = makeTaskDictionary(task = task)
    checkParamSet(par.set = par.set, dict = dict)
    par.set = evaluateParamSet(par.set = par.set, dict = dict)
    ## assure that the value names are also shown if the values list was unnamed
    par.set$pars = lapply(par.set$pars, function(x) {
      if (!is.null(x$values) && is.null(names(x$values)))
        names(x$values) = unlist(x$values)
      return(x)
    })
  }
  return(par.set)
}
