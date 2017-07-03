#' @export
makeRLearner.regr.gradBoost = function() {
  makeRLearnerRegr(
    cl = "regr.gradBoost",
    package = "rpart",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "maxiter", default = 10L),
      makeDiscreteLearnerParam(id = "baselearner", values = c("rpart", "lm"), default = "rpart"),
    ),
    properties = c("numerics"),
    name = "Gradient Boosting",
    short.name = "gradBoost"
  )
}




#' @export
trainLearner.regr.gradBoost = function(.learner, .task, .subset, .weights = NULL, Qtransform = TRUE, mgcv.s.k = -1L, bs = "tp", ...) {
}

#' @export
predictLearner.regr.gradBoost = function(.learner, .model, .newdata, ...) {
}

## Helper functions
# L2 loss
l2loss = function(yhat, y) {
  0.5 * return(yhat-y)^2
}

# derivative of L2 loss
devl2loss = function(yhat, y) {
  return(yhat - y)
}

initModelL2Pred = function(X, y) {
  return mean(y)
}

trainBaseLearner = function() {

}

# gradient boosting function
gboost = function(X, y, maxiter, calcLoss = l2loss, calcDev = devl2loss, initModelPred = initModelL2Pred) {
  list.model = list()
  ##
  yagg = initModelPred(X, y)  # yagg is a vector which stores the current prediction with all the models
  ##
  for(i in 1:maxiter) {
   vec.res = calcDev(yagg, y)
   list.models[[i]] = trainBaseLearner(X = X, Y = vec.res)
   # must use a model to represent the residule here instead of a residule factor itself since we want to predict new data that does not occur in our training set!
   res.hat = predictBaseLearner(X = X, list.models[[i]])
   ss = linesearch(s, function(s, y) {
    calLoss(s * res.hat + yagg , y)}, y = y)
   list.models[[i]] = updateBaseLearner(delta.f = ss$minimum, f = list.models[[i]])
   yagg = yagg + predictBaseLearner(X = X, list.models[[i]])
  }
}


