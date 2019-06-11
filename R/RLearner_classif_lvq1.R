# FIXME: parset
#' @export
makeRLearner.classif.lvq1 = function() {
  makeRLearnerClassif(
    cl = "classif.lvq1",
    package = "class",
    par.set = makeParamSet(),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Learning Vector Quantization",
    short.name = "lvq1",
    callees = c("lvq1", "lvqinit", "lvqtest")
  )
}

#' @export
trainLearner.classif.lvq1 = function(.learner, .task, .subset, .weights = NULL, ...) {

  d = getTaskData(.task, .subset, target.extra = TRUE)
  cdbk.args = insert(list(), list(...), c("size", "k", "prior"))
  cdbk.args$x = d$data
  cdbk.args$cl = d$target
  codebk = do.call(class::lvqinit, cdbk.args)

  lvq.args = insert(list(), list(...), c("niter", "alpha"))
  lvq.args$x = d$data
  lvq.args$cl = d$target
  lvq.args$codebk = codebk
  do.call(class::lvq1, lvq.args)
}

#' @export
predictLearner.classif.lvq1 = function(.learner, .model, .newdata, ...) {
  class::lvqtest(.model$learner.model, test = .newdata, ...)
}
