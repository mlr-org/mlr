predict_nas = function(model, newdata) {
	if (model$learner$type == "classif") {
    levs = model$task.desc$class.levels
    if (model$learner$predict.type == "response")
      p = factor(rep(NA, nrow(newdata)), levels=levs)
    else
      p = matrix(as.numeric(NA), nrow=nrow(newdata), ncol=length(levs), dimnames=list(NULL, levs))
	} else {
		p = as.numeric(rep(NA, nrow(newdata)))
	}
	return(p)
}

#' @S3method print FailureModel
print.FailureModel = function(x, ...) {
  print.WrappedModel(x)
  catf("Training failed: %s", x$learner.model)
}
