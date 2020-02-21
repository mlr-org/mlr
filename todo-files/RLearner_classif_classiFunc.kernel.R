#' @export
makeRLearner.classif.classiFunc.kernel = function() {
  makeRLearnerClassif(
    cl = "classif.classiFunc.kernel",
    package = "classiFunc",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "h",
        lower = 0, upper = Inf,
        default = 1),
      # Using metricChoices since there are so many options and this
      # keeps stuff flexible.
      # metric.choices: defined in helpers_fda.R
      makeDiscreteLearnerParam(id = "metric", default = "Euclidean",
        values = metric.choices),
      # kernel.choices: defined in helpers_fda.R
      makeDiscreteLearnerParam(id = "ker", default = "Ker.norm",
        values = kernel.choices),
      makeIntegerLearnerParam(id = "nderiv", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "derived", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "deriv.method", default = "base.diff",
        values = c("base.diff", "fda.deriv.fd")),
      makeFunctionLearnerParam(id = "custom.metric",
        default = function(x, y, ...) {
          return(sqrt(sum((x - y)^2)))
        },
        requires = quote(metric == "custom.metric"),
        tunable = FALSE),
      makeFunctionLearnerParam(id = "custom.ker",
        default = function(u) {
          return(dnorm(u))
        },
        requires = quote(ker == "custom.ker"),
        tunable = FALSE),
      makeNumericLearnerParam(id = "dmin", default = 0,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax", default = 1,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin1", default = 0,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax1", default = 1,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmin2", default = 0,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "dmax2", default = 1,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t1", default = 0,
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "t2", default = 1,
        lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = ".poi", default = expression(seq(0, 1, ncol(task))),
        lower = 0, upper = 1),
      makeNumericLearnerParam(id = "p", default = 2),
      makeNumericLearnerParam(id = "lambda", lower = 0, default = 0),
      makeDiscreteLearnerParam(id = "method", default = "chol", values = c("chol", "qr")),
      makeNumericLearnerParam(id = "dfscale", lower = 0, upper = Inf, default = 1),
      makeUntypedLearnerParam(id = "custom.metric.args", default = list()),
      keys = "task",
      forbidden = expression(dmin > dmax,
        knn %% 2 == 0)
    ),
    properties = c("twoclass", "multiclass", "prob", "single.functional"),
    name = "classiFunc.kernel",
    short.name = "classiFunc.kernel",
    note = "Default for lambda (Data2fd parameter) is 3e-8/diff(as.numeric(range(argvals)))."
  )
}

#' @export
trainLearner.classif.classiFunc.kernel = function(.learner, .task, .subset, ...) {
  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE,
    functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)

  args = learnerArgsToControl("list", ...)
  if (length(args$custom.metric.args) > 0) {
    args = c(args, args$custom.metric.args)
    args$custom.metric.args = NULL
  }
  do.call(classiFunc::classiKernel, c(list(classes = d$target, fdata = fd),
    args))
}

#' @export
predictLearner.classif.classiFunc.kernel = function(.learner, .model, .newdata, ...) {
  # extract data in matrix format
  .newdata = getFunctionalFeatures(.newdata)
  predict(.model$learner.model, newdata = .newdata, predict.type = .learner$predict.type, ...)
}
