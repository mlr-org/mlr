#' @export
makeRLearner.multilabel.randomForestSRC = function() {
  makeRLearnerMultilabel(
    cl = "multilabel.randomForestSRC",
    package = "randomForestSRC",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none", "by.user")),
      makeIntegerLearnerParam(id = "mtry", default = NULL, lower = 1L,
        special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "nodesize", lower = 1L, default = NULL,
        special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "nodedepth", default = NULL,
        special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "splitrule", default = NULL,
        values = list("gini", "gini.unwt", "gini.hvwt", "random", NULL = NULL),
        special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
        requires = quote(splitrule != "random")), # nsplit is ignored and internally set to 1L for splitrule = "random"
      makeLogicalLearnerParam(id = "split.null", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random", "anti",
          "permute.ensemble", "random.ensemble", "anti.ensemble")),
      makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
        values = c("na.omit", "na.impute"), when = "both"),
      # FIXME the default in rfsrc() for na.action is na.omit
      makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
      makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
        values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L, default = NULL,
        requires = quote(bootstrap == "by.root"), special.vals = list(NULL)),
      makeDiscreteLearnerParam(id = "samptype", default = "swr", values = c("swr", "swor"),
        requires = quote(bootstrap == "by.root")),
      makeUntypedLearnerParam(id = "samp", requires = quote(bootstrap == "by.user")),
      makeNumericVectorLearnerParam(id = "xvar.wt", lower = 0, default = NULL,
        special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "var.used", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeDiscreteLearnerParam(id = "split.depth", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
      makeIntegerLearnerParam(id = "seed", default = NULL, upper = 0L,
        tunable = FALSE, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE, when = "both"), # is currently ignored
      makeLogicalLearnerParam(id = "membership", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "tree.err", default = FALSE, tunable = FALSE),
      makeUntypedLearnerParam(id = "coerce.factor", default = NULL, special.vals = list(NULL))
    ),
    par.vals = list(na.action = "na.impute"),
    properties = c("missings", "numerics", "factors", "prob", "weights"),
    name = "Random Forest",
    short.name = "rfsrc",
    note = "`na.action` has been set to `na.impute` by default to allow missing data support.",
    callees = "rfsrc"
  )
}

#' @export
trainLearner.multilabel.randomForestSRC = function(.learner, .task, .subset, .weights = NULL, ...) {
  targets = getTaskTargetNames(.task)
  f = as.formula(stri_paste("cbind(", stri_paste(targets, collapse = ",", sep = " "), ")  ~ .", sep = ""))
  d = getTaskData(.task, .subset, recode.target = "multilabel.factor")
  randomForestSRC::rfsrc(f, data = d, case.wt = .weights, ...)
}

#' @export
predictLearner.multilabel.randomForestSRC = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, importance = "none", ...)
  if (.learner$predict.type == "prob") {
    return(sapply(p$classOutput, function(x) x$predicted[, 1]))
  } else {
    return(sapply(p$classOutput, function(x) as.logical(x$class)))
  }
}
