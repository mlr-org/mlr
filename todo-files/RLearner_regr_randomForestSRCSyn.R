#' @export
makeRLearner.regr.randomForestSRCSyn = function() {
  makeRLearnerRegr(
    cl = "regr.randomForestSRCSyn",
    package = "randomForestSRC",
    par.set = makeParamSet(
      ## arguments of rfsrcSyn
      ## only ntree, mtrySeq, nodesizeSeq, nsplit are relevant to the RF machines
      ## all other params apply to the synthetic forest
      makeIntegerLearnerParam(id = "ntree", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerVectorLearnerParam(id = "mtrySeq", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerVectorLearnerParam(id = "nodesizeSeq", default = c(1L:10L, 20L, 30L, 50L, 100L)),
      makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
        requires = quote(splitrule != "random")),
        # for the synthetic forest nsplit is ignored and internally set to 1L if splitrule = "random"
        # splitrule cannot be set for the RF machines, so if nsplit != 0 mse splitting with nsplit randomly selected split points is done
      makeNumericLearnerParam(id = "min.node", default = 3L, lower = 0L),
      makeLogicalLearnerParam(id = "use.org.features", default = TRUE),
      makeDiscreteLearnerParam(id = "na.action", default = "na.omit",
        values = c("na.omit", "na.impute"), when = "both"),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, tunable = FALSE),
      ## further arguments to rfsrc (synthetic forest) via ...
      makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
        values = c("by.root", "by.node", "none")),
      makeIntegerLearnerParam(id = "nodedepth", default = -1L),
      makeDiscreteLearnerParam(id = "splitrule", default = "mse",
        values = c("mse", "mse.unwt", "mse.hvwt", "random")),
      makeLogicalLearnerParam(id = "split.null", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
        values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random", "anti",
          "permute.ensemble", "random.ensemble", "anti.ensemble")),
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
    par.vals = list(na.action = "na.impute", verbose = FALSE),
    properties = c("numerics", "factors", "ordered", "missings"),
    name = "Synthetic Random Forest",
    short.name = "rfsrcSyn",
    note = '`na.action` has been set to `"na.impute"` by default to allow missing data support and `verbose` has been set to `FALSE`.'
    )
}

#' @export
trainLearner.regr.randomForestSRCSyn = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  randomForestSRC::rfsrcSyn(formula = f, data = getTaskData(.task, .subset), case.wt = .weights, ...)
}

#' @export
predictLearner.regr.randomForestSRCSyn = function(.learner, .model, .newdata, ...) {
  p = randomForestSRC::rfsrcSyn(object = .model$learner.model, newdata = .newdata, membership = FALSE, ...)$rfSynPred$predicted
  # version > 2.0 of randomForestSRC returns an array here :(
  as.numeric(p)
}
