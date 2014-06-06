#' @export
makeRLearner.surv.glmnet = function() {
  makeRLearnerSurv(
    cl = "surv.glmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="nfolds", default=10L, lower=3L), # FIXME: upper=nrow?
      makeNumericLearnerParam(id="alpha", default=1, lower=0, upper=1)
    ),
    properties = c("numerics", "weights", "rcens")
  )
}

#' @export
trainLearner.surv.glmnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
  #FIXME: unnecessary data duplication
  data = getTaskData(.task, subset=.subset, target.extra=TRUE, recode.target="surv")
  if (is.null(.weights)) {
    cv.glmnet(y=data$target, x=as.matrix(data$data), family="cox", ...)
  } else  {
    cv.glmnet(y=data$target, x=as.matrix(data$data), weights=.weights, family="cox", ...)
  }
}

#' @export
predictLearner.surv.glmnet = function(.learner, .model, .newdata, ...) {
  s = .model$learner.model$lambda.min
  if(.learner$predict.type == "response")
    as.numeric(predict(.model$learner.model, newx=as.matrix(.newdata), type="link", s=s, ...))
  else
    stop("Unknown predict type")
}
