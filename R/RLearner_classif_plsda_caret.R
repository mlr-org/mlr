#' @export
makeRLearner.classif.plsda_caret = function() {
  makeRLearnerClassif(cl = "classif.plsda_caret",
                      package = "caret",
                      par.set = makeParamSet(
                        makeIntegerLearnerParam(id = "ncomp", default = 2, lower = 1),
                        makeDiscreteLearnerParam(id = 'probMethod', values = c('softmax', 'Bayes'), default = 'softmax')
                      ),
                      properties = c('numerics', 'prob', 'twoclass'))
}

#' @export
trainLearner.classif.plsda_caret = function(.learner, .task, .subset, .weights, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  plsda(d$data, d$target, method = "oscorespls", ...)
}

#' @export
predictLearner.classif.plsda_caret = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == 'response', 'class', 'prob')
  p = predict(.model$learner.model, newdata = .newdata, type = type, ...)
  if(type == 'prob'){
    p = p[,,1]
  }
  return(p)
}