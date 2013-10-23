makeChainModel = function(next.model, cl) {
  #fixme: what to do in case of failure?
  structure(list(
    next.model = next.model
  ), class=c(cl, "ChainModel", "WrappedModel"))
}


#' @S3method makeWrappedModel BaseWrapper
makeWrappedModel.BaseWrapper = function(learner, model, task.desc, subset, features, time) {
  x = NextMethod()
  class(x) = c("BaseWrapperModel", class(x))
  return(x)
}

#' @S3method print BaseWrapperModel
print.BaseWrapperModel = function(x, ...) {
  leafm = getLeafModel(x)
  catf("Learner model for id=%s chain=%s", 
    x$learner$id, class(x$learner)[1])
  catf("[chain] Trained on obs: %i", length(x$subset))
  catf("[chain] Used features: %i", length(x$features))
  catf("[model] Trained on obs: %i", length(leafm$subset))
  catf("[model] Used features: %i", length(leafm$features))
  catf("Hyperparameters: %s", getHyperParsString(x$learner))
}

