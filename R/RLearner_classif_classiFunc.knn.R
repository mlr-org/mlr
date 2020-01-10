#' @export
makeRLearner.classif.classiFunc.knn = function() {
  makeRLearnerClassif(
    cl = "classif.classiFunc.knn",
    package = "classiFunc",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "knn", lower = 1L, default = 1L),
      # Using metricChoices since there are so many options and this
      # keeps stuff flexible.
      makeDiscreteLearnerParam(id = "metric", default = "Euclidean",
        values = metric.choices),
      makeIntegerLearnerParam(id = "q?", default = 0L, lower = 0L),
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
      makeNumericLearnerParam(id = "dtwwindow", lower = 0, upper = 1, requires = quote(metric == "rucrdtw")),
      makeNumericLearnerParam(id = "lambda", lower = 0, default = 0),
      makeDiscreteLearnerParam(id = "method", default = "chol", values = c("chol", "qr")),
      makeNumericLearnerParam(id = "dfscale", lower = 0, upper = Inf, default = 1),
      makeUntypedLearnerParam(id = "custom.metric.args", default = list()),
      keys = "task",
      forbidden = expression(
        dmin >= dmax,
        dmin1 >= dmax1 | dmin2 >= dmax2,
        knn %% 2 == 0)
    ),
    properties = c("twoclass", "multiclass", "prob", "single.functional"),
    name = "classiFunc.knn",
    short.name = "classiFunc.knn",
    note = "Default for lambda (Data2fd parameter) is 3e-8/diff(as.numeric(range(argvals)))."
  )
}

#' @export
trainLearner.classif.classiFunc.knn = function(.learner, .task, .subset, ...) {
  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)

  args = learnerArgsToControl("list", ...)
  if (length(args$custom.metric.args) > 0) {
    args = c(args, args$custom.metric.args)
    args$custom.metric.args = NULL
  }
  do.call(classiFunc::classiKnn, c(list(classes = d$target, fdata = fd), args))
}

#' @export
predictLearner.classif.classiFunc.knn = function(.learner, .model, .newdata, ...) {
  # extract data in matrix format
  .newdata = getFunctionalFeatures(.newdata)
  predict(.model$learner.model, newdata = .newdata, predict.type = .learner$predict.type, ...)
}
