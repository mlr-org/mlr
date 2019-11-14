boosting_helper1 = function(formula, data, subset = seq_len(nrow(data)), ...) {
  args = list(...)
  if (!is.null(args$cp)) {
    ctrl = rpart::rpart.control(cp = args$cp, xval = 0)
  } else {
    ctrl = rpart::rpart.control(xval = 0)
  }
  set.seed(getOption("mlr.debug.seed"))
  adabag::boosting(formula, data[subset, ], mfinal = args$mfinal, control = ctrl)
}

boosting_helper2 = function(model, newdata) {
  set.seed(getOption("mlr.debug.seed"))
  as.factor(predict(model, newdata)$class)
}
