predict_nas = function(model, newdata) {
  if (model$learner$type == "classif") {
    levs = model$task.desc$class.levels
    if (model$learner$predict.type == "response")
      p = factor(rep(NA_character_, nrow(newdata)), levels=levs)
    else
      p = matrix(NA_real_, nrow=nrow(newdata), ncol=length(levs), dimnames=list(NULL, levs))
  } else {
    p = rep(NA_real_, nrow(newdata))
  }
  return(p)
}


#' @export
print.FailureModel = function(x, ...) {
  print.WrappedModel(x)
  catf("Training failed: %s", x$learner.model)
}
