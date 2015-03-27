

# library(caTools)
# LogitBoost
# nIter: An integer, describing the number of iterations for which boosting should be run, 
#         or number of decision stumps that will be used.

#' @export
makeRLearner.classif.lb = function() {
  makeRLearnerClassif(
    cl = "classif.lb",
    package = "caTools",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "nIter", lower = 2)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "LogitBoost",
    short.name = "LB",
  )
}

#' @export
trainLearner.classif.lb = function(.learner, .task, .subset, .weights,  ...) {
  f <- getTaskFormula(.task)
  data <- getTaskData(.task, .subset)
  target <- as.character(f[[2]])
  tgt <- which(names(data) == target)
  X <- data[ ,-tgt]
  Y <- data[ ,tgt]
  LogitBoost(X, Y, ...)
}


#' @export
predictLearner.classif.lb = function(.learner, .model, .newdata, ...) {
  
  if(.learner$predict.type == "response")
    outType <- "class"
  else
    outType <- "raw"  #returns posterior probability
  
  #if (.learner$predict.type == "prob") 
  #     outType <- "raw"
      
  output <- predict(.model$learner.model, xtest = .newdata, type = outType, ...)
  return(output)
}
