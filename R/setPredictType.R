#' @title Set the type of predictions the learner should return.
#'
#' @description
#' Possible prediction types are:
#' Classification: Labels or class probabilities (including labels).
#' Regression: Numeric or response or standard errors (including numeric response).
#' Survival: Linear predictor or survival probability.
#'
#' For complex wrappers the predict type is usually also passed down the
#' encapsulated learner in a recursive fashion.
#'
#' @template arg_learner
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} or \dQuote{prob}.
#'   Regression: \dQuote{response} or \dQuote{se}.
#'   Survival: \dQuote{response} (linear predictor) or \dQuote{prob}.
#'   Clustering: \dQuote{response} or \dQuote{prob}.
#'   Default is \dQuote{response}.
#' @template ret_learner
#' @family predict
#' @family learner
#' @export
setPredictType = function(learner, predict.type) {
  assertClass(learner, classes = "Learner")
  UseMethod("setPredictType")
}

#' @export
setPredictType.Learner = function(learner, predict.type) {
  # checks should be done down here i guess, because of recursive calls in wrappers
  assertChoice(predict.type, choices = switch(learner$type,
    classif = c("response", "prob"),
    multilabel = c("response", "prob"),
    regr = c("response", "se"),
    surv = c("response", "prob"),
    costsens = "response",
    cluster = c("response", "prob"),
    fcregr = c("response", "quantile"),
    mfcregr = c("response", "quantile")
  ))
  if (predict.type == "prob" && !hasLearnerProperties(learner, "prob"))
    stopf("Trying to predict probs, but %s does not support that!", learner$id)
  if (predict.type == "se" && !hasLearnerProperties(learner, "se"))
    stopf("Trying to predict standard errors, but %s does not support that!", learner$id)
  if (predict.type == "quantile" && !hasLearnerProperties(learner, "quantile"))
    stopf("Trying to predict quantiles, but %s does not support that!", learner$id)
  learner$predict.type = predict.type
  return(learner)
}
