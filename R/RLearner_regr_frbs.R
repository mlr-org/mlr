#' @export
makeRLearner.regr.frbs = function() {
  makeRLearnerRegr(
    cl = "regr.frbs",
    package = "frbs",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "WM",
        values = c("WM", "SBC", "HYFIS", "ANFIS", "DENFIS", "FS.HGD", "FIR.DM", "GFS.FR.MOGUL", "GFS.THRIFT", "GFS.LT.RS")),
      makeIntegerLearnerParam(id = "num.labels", default = 7L, lower = 1L,
        requires = quote(method %in% c("WM", "HYFIS", "ANFIS", "FS.HGD", "FIR.DM", "GFS.LT.RS"))),
      makeDiscreteLearnerParam(id = "type.mf", default = "GAUSSIAN",
        values = c("TRIANGLE", "TRAPEZOID", "GAUSSIAN", "SIGMOID", "BELL"),
        requires = quote(method == "WM")),
      makeDiscreteLearnerParam(id = "type.defuz", default = "WAM",
        values = c("WAM", "FIRST.MAX", "LAST.MAX", "MEAN.MAX", "COG"),
        requires = quote(method %in% c("WM", "HYFIS", "GFS.THRIFT", "GFS.LT.RS"))),
      makeDiscreteLearnerParam(id = "type.tnorm", default = "MIN",
        values = c("MIN", "HAMACHER", "YAGER", "PRODUCT", "BOUNDED"),
        requires = quote(method %in% c("WM", "HYFIS", "ANFIS", "FS.HGD", "FIR.DM", "GFS.THRIFT", "GFS.LT.RS"))),
      makeDiscreteLearnerParam(id = "type.snorm", default = "MAX",
        values = c("MAX", "HAMACHER", "YAGER", "PRODUCT", "BOUNDED"),
        requires = quote(method %in% c("WM", "HYFIS", "ANFIS", "FS.HGD", "FIR.DM", "GFS.THRIFT", "GFS.LT.RS"))),
      makeDiscreteLearnerParam(id = "type.implication.func", default = "ZADEH",
        values = c("DIENES_RESHER", "LUKASIEWICZ", "ZADEH", "GOGUEN", "GODEL", "SHARP", "MIZUMOTO", "DUBOIS_PRADE", "MIN"),
        requires = quote(method %in% c("WM", "HYFIS", "ANFIS", "FS.HGD", "GFS.THRIFT", "GFS.LT.RS"))),
      makeIntegerLearnerParam(id = "max.iter", default = 10L, lower = 1L,
        requires = quote(method %in% c("HYFIS", "ANFIS", "FS.HGD", "FIR.DM", "GFS.FR.MOGUL"))),
      makeNumericLearnerParam(id = "step.size", default = 0.01, lower = 0, upper = 1,
        requires = quote(method %in% c("HYFIS", "ANFIS", "FIR.DM", "FS.HGD", "DENFIS"))),
      makeNumericLearnerParam(id = "r.a", default = 0.5, lower = .Machine$double.eps,
        requires = quote(method == "SBC")),
      makeNumericLearnerParam(id = "eps.high", default = 0.5, lower = .Machine$double.eps,
        requires = quote(method == "SBC")),
      makeNumericLearnerParam(id = "eps.low", default = 0.15, lower = .Machine$double.eps,
        requires = quote(method == "SBC")),
      makeNumericLearnerParam(id = "alpha.heuristic", default = 1, lower = .Machine$double.eps,
        requires = quote(method == "FS.HGD")),
      makeNumericLearnerParam(id = "Dthr", default = 0.1, lower = 0, upper = 1,
        requires = quote(method == "DENFIS")),
      makeNumericLearnerParam(id = "d", default = 2, requires = quote(method == "DENFIS")),
      makeNumericLearnerParam(id = "persen_cross", default = 1, lower = 0, upper = 1,
        requires = quote(method %in% c("GFS.FR.MOGUL", "GFS.THRIFT"))),
      makeIntegerLearnerParam(id = "max.gen", default = 10L, lower = 1L,
        requires = quote(method %in% c("GFS.FR.MOGUL", "GFS.THRIFT", "GFS.LT.RS"))),
      makeIntegerLearnerParam(id = "max.tune", default = 10L, lower = 1L,
        requires = quote(method == "GFS.FR.MOGUL")),
      makeNumericLearnerParam(id = "persen_mutant", default = 1, lower = 0, upper = 1,
        requires = quote(method %in% c("GFS.FR.MOGUL", "GFS.THRIFT", "GFS.LT.RS"))),
      makeNumericLearnerParam(id = "epsilon", default = 0.9, lower = 0, upper = 1,
        requires = quote(method == "GFS.FR.MOGUL")),
      makeIntegerLearnerParam(id = "popu.size", default = 10L, lower = 1L,
        requires = quote(method %in% c("GFS.THRIFT", "GFS.LT.RS"))),
      makeDiscreteLearnerParam(id = "mode.tuning", default = "GLOBAL", values = c("GLOBAL", "LOCAL"),
        requires = quote(method == "GFS.LT.RS")),
      makeLogicalLearnerParam(id = "rule.selection", default = FALSE,
        requires = quote(method == "GFS.LT.RS"))
    ),
    properties = "numerics",
    name = "Fuzzy Rule-based Systems",
    short.name = "frbs",
    callees = "frbs.learn"
  )
}

#' @export
trainLearner.regr.frbs = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  args = list(...)
  method.arg = names(args) == "method"
  if (any(method.arg)) {
    args = list(method = args$method, control = args[!method.arg])
  } else {
    args = list(control = args)
  }
  args$data = cbind(d$data, d$target)
  do.call(frbs::frbs.learn, args)
}

#' @export
predictLearner.regr.frbs = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}
