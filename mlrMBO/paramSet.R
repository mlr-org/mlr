getParamSet = function() {
  library(ParamHelpers)

  makeParamSet(
    makeDiscreteParam("model", values = c("multinom", "rpart", "randomForest")),

    # multinom
    # -> no parameters

    # rpart
    makeIntegerParam("rpart.minsplit", lower=1L, upper=30L,
      requires = quote(model == "rpart")),
    makeIntegerParam("rpart.minbucket", lower=1L, upper=15L,
      requires = quote(model == "rpart")),
    makeNumericParam("rpart.cp", lower=0.001, upper=0.1,
      requires = quote(model == "rpart")),

    # randomForest
    makeIntegerParam("randomForest.ntree", lower=10L, upper=1000L,
      requires = quote(model == "randomForest")),
    makeIntegerParam("randomForest.nodesize", lower=1L, upper=10L,
      requires = quote(model == "randomForest")),
    makeLogicalParam("randomForest.replace",
      requires = quote(model == "randomForest"))
  )
}
