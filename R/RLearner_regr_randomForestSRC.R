#' @export
makeRLearner.regr.randomForestSRC = function() {
  makeRLearnerRegr(
    cl = "regr.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 5L),
      makeIntegerLearnerParam(id = "nodedepth", default = -1L),
      makeDiscreteLearnerParam(id = "splitrule", default = "mse",
        values = c("mse", "mse.unwt", "mse.hvwt", "random")),
      makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
        requires = quote(splitrule != "random")), # nsplit is ignored and internally set to 1L for splitrule = "random"
      makeLogicalLearnerParam(id = "split.null", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random", "anti",
          "permute.ensemble", "random.ensemble", "anti.ensemble")),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      # FIXME default in rfsrc() for na.action is na.omit
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
        values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L,
        requires = quote(bootstrap == "by.root")),
      makeDiscreteLearnerParam(id = "samptype", default = "swr", values = c("swr", "swor"),
        requires = quote(bootstrap == "by.root")),
      makeNumericVectorLearnerParam(id = "xvar.wt", lower = 0),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "var.used", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeDiscreteLearnerParam(id = "split.depth", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeIntegerLearnerParam(id = "seed", upper = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE, when = "both"), # is currently ignored
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "tree.err", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "ordered", "weights", "oobpreds", "featimp"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = '`na.action` has been set to `"na.impute"` by default to allow missing data support.',
    callees = "rfsrc"
  )
}

#' @export
trainLearner.regr.randomForestSRC = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = getTaskData(.task, .subset), case.wt = .weights, ...)
}

#' @export
predictLearner.regr.randomForestSRC = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, membership = FALSE, ...)
  # versison 2.0 of randomForestSRC returns an array here :(
  as.numeric(p$predicted)
}

#' @export
getOOBPredsLearner.regr.randomForestSRC = function(.learner, .model) {
  as.numeric(getLearnerModel(.model, more.unwrap = TRUE)$predicted.oob)
}

#' @rdname getFeatureImportanceLearner
getFeatureImportanceLearner.regr.randomForestSRC = function(.learner, .model, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  randomForestSRC::vimp(mod)$importance
}
