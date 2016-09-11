#'@export
makeRLearner.mfcregr.BigVAR = function() {
  makeRLearnerMultiForecastRegr(
    cl = "mfcregr.BigVAR",
    package = "BigVAR",
    par.set = makeParamSet(
      #constructModel params
      # p, struct, and gran have no default
      makeIntegerLearnerParam("p", lower = 1L, default = 1L),
      makeDiscreteLearnerParam("struct", values = c("Basic", "Lag", "SparseLag", "SparseOO",
                               "OwnOther", "EFX", "HVARC", "HVAROO", "HVARELEM", "Tapered")),
      makeIntegerVectorLearnerParam("gran", len = 2L, lower = 1L),
      makeLogicalLearnerParam("RVAR", default = FALSE),
      makeNumericLearnerParam("h", lower = 1, default = 1),
      makeDiscreteLearnerParam("cv", values = c("Rolling", "LOO"), default = "Rolling"),
      makeLogicalLearnerParam("MN", default = FALSE),
      makeLogicalLearnerParam("verbose", default = FALSE),
      makeLogicalLearnerParam("IC", default = TRUE),
      makeIntegerLearnerParam("T1", lower = 1L, default = expression(floor(nrow(Y) / 3))),
      makeIntegerLearnerParam("T2", lower = 1L, default = expression(2 * floor(nrow(Y) / 3))),
      makeLogicalLearnerParam("ONESE", default = FALSE),
      makeLogicalLearnerParam("ownlambdas", default = FALSE),
      makeUntypedLearnerParam("alpha", default = as.double(NULL)),
      makeLogicalLearnerParam("recursive", default = FALSE),
      makeUntypedLearnerParam("C", default = as.double(NULL)),
      # predictor vars
      makeIntegerLearnerParam("n.ahead", lower = 1, default = 1, when = "predict"),
      keys = "Y"
    ),
    properties = "numerics",
    name = "Vector AutoRegression Models with Endogeneous and Exogenous Variables",
    short.name = "BigVAR",
    note = "Because of argument p and R's partial matching, arguments in makeLearner must be passed to par.vals as a list. dates are automatically passed to the date argument.",
    callees = c("constructModel", "cv.BigVAR")
  )
}

#'@export
trainLearner.mfcregr.BigVAR = function(.learner, .task, .subset, .weights = NULL, ...) {
  data  = getTaskData(.task, .subset)
  dates = rownames(data)
  data  = as.matrix(data)
  model = BigVAR::constructModel(Y = data, ..., dates = dates)
  BigVAR::cv.BigVAR(model)
}

#'@export
predictLearner.mfcregr.BigVAR = function(.learner, .model, .newdata, ...) {

  p = lapply(1:I(.model$learner.model@horizon), function(i) {
    as.data.frame(BigVAR::predict(.model$learner.model, i))
  })
  p[[1]] = as.data.frame(t(p[[1]]))
  p = rbindlist(p)
  colnames(p) = .model$task.desc$col.names
  return(as.matrix(p))
}



