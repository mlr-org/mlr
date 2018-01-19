#' @export
makeRLearner.oneclass.lofactor = function() {
  makeRLearnerOneClass(
    cl = "oneclass.lofactor",
    package = "DMwR",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", default = 20L, lower = 1L, tunable = TRUE)
    ),
    properties =  c("oneclass", "numerics", "factors", "prob"),
    note = "Rule of thumb for number of neighbours k = 20,
    see http://scikit-learn.org/stable/modules/outlier_detection.html.",
    name = "LOF DMwR",
    short.name = "lofactor",
    callees = "lofactor"
  )
}

#' @export
trainLearner.oneclass.lofactor = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE)
  DMwR::lofactor(z$data, ...)
}

#' @export
predictLearner.oneclass.lofactor = function(.learner, .model, .newdata, ...) {
  # calculate lof, no trained model is needed
  # the lower the local density of a point -> the point is in a sparser region than its neighbors, which suggests that the point is an outlier.
  if(is.null(.learner$par.vals$k)) {
    k = .model$learner$par.set$pars$k$default
  } else if (!is.null(.learner$par.vals$k)) {
    k = .learner$par.vals$k
  } else {
    k = .model$learner$par.vals$k
  }
  p.df = DMwR::lofactor(.newdata, k = k, ...)
  message("lof method is unsupervised, therefore predict()-fct trains and predicts on the same data")
  #p.df.old = .model$learner.model
  #threshold = min(p.df.old)
  td = getTaskDesc(.model)
  label = c(td$positive, td$negative)
  if (.learner$predict.type == "response") {
    indices.threshold = order(p.df)[round(length(p.df) * 0.95)]  # mse reconstruction error in [0,inf[
    predict.threshold = p.df[indices.threshold]
    p = p.df >= predict.threshold
    p = factor(p, levels = c("TRUE", "FALSE"), labels = label)
  } else {
    #lof-score low = low density = sparse in comparison to neighbour = high lof score = likely to be anomaly
    #convertingScoresToProbability() is monoton increasing trafo
    #def of convertingScoresToProbability() is high prob = anomaly
    #therefore use 1-p to indicate high prob = anomaly
    p = convertingScoresToProbability(p.df)$probability
    p = cbind(p, 1 - p)
    colnames(p) = label
  }
  return(p)
}
