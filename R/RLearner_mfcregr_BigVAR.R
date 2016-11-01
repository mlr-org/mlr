#'@export
makeRLearner.mfcregr.BigVAR = function() {
  makeRLearnerMultiForecastRegr(
    cl = "mfcregr.BigVAR",
    package = "BigVAR",
    par.set = makeParamSet(
      #constructModel params
      makeIntegerLearnerParam("p", lower = 1L, default = 1L),
      makeDiscreteLearnerParam("struct", values = c("Basic", "Lag","SparseLag","SparseOO",
                               "OwnOther", "EFX", "HVARC", "HVAROO", "HVARELEM", "Tapered")),
      makeIntegerVectorLearnerParam("gran", len = 2L, lower = 1L),
      makeLogicalLearnerParam("RVAR", default = FALSE),
      makeNumericLearnerParam("h", lower = 1, default = 1),
      makeDiscreteLearnerParam("cv", values = c("Rolling", "LOO")),
      makeLogicalLearnerParam("MN", default = FALSE),
      makeLogicalLearnerParam("verbose", default = FALSE),
      makeLogicalLearnerParam("IC", default = TRUE),
      makeIntegerLearnerParam("T1", lower = 1L),
      makeIntegerLearnerParam("T2", lower = 1L),
      makeLogicalLearnerParam("ONESE", default = FALSE),
      makeLogicalLearnerParam("ownlambdas", default = FALSE),
      makeUntypedLearnerParam("alpha", default = as.double(NULL)),
      makeLogicalLearnerParam("recursive", default = FALSE),
      makeUntypedLearnerParam("C", default = as.double(NULL)),
      # predictor vars
      makeIntegerLearnerParam("n.ahead", lower = 1, default = 1, when = "predict")
    ),
    properties = c("numerics"),
    name = "Vector AutoRegression Models with Endogeneous and Exogenous Variables",
    short.name = "BigVAR",
    note = ""
  )
}

#'@export
trainLearner.mfcregr.BigVAR = function(.learner, .task, .subset, .weights = NULL, ...) {
  dots = list(...)
  data  = getTaskData(.task,.subset)
  dates = rownames(data)
  data  = as.matrix(data)
  model = BigVAR::constructModel(Y = data, ..., dates = dates)
  BigVAR::cv.BigVAR(model)
}

#'@export
predictLearner.mfcregr.BigVAR = function(.learner, .model, .newdata, ...){

  p = lapply(1:I(.model$learner.model@horizon), function(i){
    BigVAR::predict(.model$learner.model, i)
  })
  p[[1]] = t(p[[1]])
  p = do.call(rbind,p)
  colnames(p) = .model$task.desc$col.names
  return(p)
}



