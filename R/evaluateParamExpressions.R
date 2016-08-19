#' @title Evaluates expressions within a learner or parameter set.
#'
#' @description
#' A \code{\link{Learner}} can contain unevaluated \code{\link[base]{expression}s}
#' as value for a hyperparameter. E.g., these expressions are used if the default
#' value depends on the task size or an upper limit for a parameter is given by
#' the number of features in a task. \code{evaluateParamExpressions} allows to
#' evaluate these expressions using a given dictionary, which holds the following
#' information:
#' \itemize{
#'   \item{\code{task}:} the task itself, allowing to access any of its elements.
#'   \item{\code{p}:} the number of features in the task
#'   \item{\code{n}:} the number of observations in the task
#'   \item{\code{type}:} the task type, i.e. "classif", "regr", "surv", "cluster", "costcens" or "multilabel"
#'   \item{\code{k}:} the number of classes of the target variable (only available for classification tasks)
#' }
#' Usually the evaluation of the expression is performed automatically, e.g. in
#' \code{\link{train}} or \code{\link{tuneParams}}. Therefore calling
#' \code{evaluateParamExpressions} manually should not be necessary.
#' It is also possible to directly evaluate the expressions of a
#' \code{\link[ParamHelpers]{ParamSet}}, \code{\link[base]{list}} of
#' \code{\link[ParamHelpers]{Param}s} or single \code{\link[ParamHelpers]{Param}s}.
#' For further information on these, please refer to the documentation of the
#' \code{ParamHelpers} package.
#'
#' @param obj [\code{\link{Learner}}]\cr
#'   The learner. If you pass a string the learner will be created via
#'   \code{\link{makeLearner}}. Expressions within \code{length}, \code{lower}
#'   or \code{upper} boundaries, \code{default} or \code{value} will be
#'   evaluated using the provided dictionary (\code{dict}).
#' @param dict [\code{environment} | \code{list} | \code{NULL}]\cr
#'   Environment or list which will be used for evaluating the variables
#'   of expressions within a parameter, parameter set or list of parameters.
#'   The default is \code{NULL}.
#' @return [\code{\link{Learner}}].
#' @export
#' @examples
#' ## (1) evaluation of a learner's hyperparameters
#' task = makeClassifTask(data = iris, target = "Species")
#' dict = getTaskDictionary(task = task)
#' lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p),
#'   minbucket = expression(3L + 4L * task$task.desc$has.blocking))
#' lrn2 = evaluateParamExpressions(obj = lrn1, dict = dict)
#'
#' getHyperPars(lrn1)
#' getHyperPars(lrn2)
#'
#' ## (2) evaluation of a learner's entire parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' dict = getTaskDictionary(task = task)
#' lrn1 = makeLearner("classif.randomForest")
#' lrn2 = evaluateParamExpressions(obj = lrn1, dict = dict)
#'
#' ## Note the values for parameters 'mtry', 'classwt' and 'cutoff'
#' lrn1$par.set
#' lrn2$par.set
#'
#' ## (3) evaluation of a parameter set
#' task = makeClassifTask(data = iris, target = "Species")
#' dict = getTaskDictionary(task = task)
#' ps1 = makeParamSet(
#'   makeNumericParam("C", lower = expression(k), upper = expression(n), trafo = function(x) 2^x),
#'   makeDiscreteParam("sigma", values = expression(list(k, p)))
#' )
#' ps2 = evaluateParamExpressions(obj = ps1, dict = dict)
#'
#' ps1
#' ps2
evaluateParamExpressions.Learner = function(obj, dict = NULL) {
  obj = checkLearner(obj)
  if (hasExpression(obj)) {
    assertList(dict, null.ok = TRUE)
    obj$par.set = evaluateParamExpressions(obj = obj$par.set, dict = dict)
    obj$par.vals = evaluateParamExpressions(obj = obj$par.vals, dict = dict)
  }
  return(obj)
}
