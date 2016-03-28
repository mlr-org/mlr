#' @export
makeRLearner.classif.randomForestSRC = function() {
  makeRLearnerClassif(
    cl = "classif.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeNumericLearnerParam(id = "mtry.ratio", lower = 0L, upper = 1L),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nodedepth", default = -1L),
      makeDiscreteLearnerParam(id = "splitrule", default = "gini",
        values = c("gini", "gini.unwt", "gini.hvwt", "random")),
      makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
        requires = quote(splitrule != "random")), # nsplit is ignored and internally set to 1L for splitrule = "random"
      makeLogicalLearnerParam(id = "split.null", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random", "anti",
          "permute.ensemble", "random.ensemble", "anti.ensemble")),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute", "na.random"), when = "both"),
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
        values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
      makeNumericVectorLearnerParam(id = "xvar.wt", lower = 0),
      makeDiscreteLearnerParam(id = "var.used", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeDiscreteLearnerParam(id = "split.depth", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeIntegerLearnerParam(id = "seed", upper = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE, when = "both"), # is currently ignored
      makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "fast.restore", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "ordered", "prob", "twoclass", "multiclass"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = '`na.action` has been set to `"na.impute"` by default to allow missing data support.
      `mtry.ratio` indicates the proportion of variables randomly selected as candidates for a split.'
  )
}

#' @export
trainLearner.classif.randomForestSRC = function(.learner, .task, .subset, .weights = NULL, mtry = NULL, mtry.ratio = NULL, ...) {
  if (!is.null(mtry.ratio)) {
    if (!is.null(mtry))
      stop("You cannot set both 'mtry' and 'mtry.ratio'")
    mtry = mtry.ratio * getTaskNFeats(.task)
  }
  f = getTaskFormula(.task)
  randomForestSRC::rfsrc(f, data = getTaskData(.task, .subset, recode.target = "drop.levels"), forest = TRUE, mtry = mtry, ...)
}

#' @export
predictLearner.classif.randomForestSRC = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, membership = FALSE, ...)
  if (.learner$predict.type == "prob") {
    return(p$predicted)
  } else {
    return(p$class)
  }
}
