#' @export
makeRLearner.classif.h2o.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.h2o.gbm",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 5L),
      makeIntegerLearnerParam("min_rows", lower = 1L, default = 10L),
      makeNumericLearnerParam("learn_rate", lower = 0, upper = 1, default = 0.1),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024L),
      makeLogicalLearnerParam("balance_classes", default = FALSE),
      makeIntegerLearnerParam("max_after_balance_size", lower = 0L, default = 1L),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "missings", "featimp"),
    name = "h2o.gbm",
    short.name = "h2o.gbm",
    note = "'distribution' is set automatically to 'gaussian'.",
    callees = "h2o.gbm"
  )
}

#' @export
trainLearner.classif.h2o.gbm = function(.learner, .task, .subset, .weights = NULL, ...) {

  params = list(...)
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) {
    return(FALSE)
  })
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  params$y = getTaskTargetNames(.task)
  params$x = getTaskFeatureNames(.task)
  params$training_frame = getTaskData(.task, subset = .subset)

  if (!is.null(.weights)) {
    params$weights_column = .weights
  }

  params$training_frame = h2o::as.h2o(params$training_frame)
  params$distribution = ifelse(length(getTaskDesc(.task)$class.levels) == 2L, "bernoulli", "multinomial")

  model = do.call(h2o::h2o.gbm, params)
  return(model)
}

#' @export
predictLearner.classif.h2o.gbm = function(.learner, .model, .newdata, ...) {

  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)

  # check if class names are integers. if yes, colnames of p.df need to be adapted
  int = stri_detect_regex(p.df$predict, "^[[:digit:]]+$")
  pcol = stri_detect_regex(colnames(p.df), "^p[[:digit:]]+$")
  if (any(int) && any(pcol)) {
    colnames(p.df)[pcol] = stri_sub(colnames(p.df)[pcol], 2L)
  }

  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    return(as.matrix(p.df))
  }
}

#' @export
getFeatureImportanceLearner.classif.h2o.gbm = function(.learner, .model, ...) {
  mod = getLearnerModel(.model, more.unwrap = TRUE)
  extractH2OVarImp(mod, ...)
}
