#' @export
makeRLearner.tsclassif.shapeletClass = function() {
  makeRLearnerClassif(
    cl = "tsclassif.shapeletClass",
    package = "shapeletLib",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "hinge", values = c("hinge", "log")),
      makeNumericLearnerParam(id = "K", default = 0.02, lower = 0.01, upper = 0.3),
      makeNumericLearnerParam(id = "L", default = 0.1, lower = 0.1, upper = 1),
      makeNumericLearnerParam(id = "lambda", lower = 0, upper = 1),
      makeNumericLearnerParam(id = "C", default = 1, lower = 0, upper = Inf),
      makeDiscreteLearnerParam(id = "step", default = "sqrt", values = c("sqrt", "step.size")),
      makeDiscreteLearnerParam(id = "init.method", default = "kmeans", values = c("kmeans", "random")),
      makeLogicalLearnerParam(id = "auto.hinge", default = TRUE)
    ),
 #   par.vals = list(K = 0.01, L = 0.1),
    #properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "ordered", "prob", "weights", "featimp"),
    properties = c("twoclass", "numerics"),
    name = "Shapelet Learning for Classification",
    short.name = "shapeletClass"
  )
}

#' @export
trainLearner.tsclassif.shapeletClass = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "-1+1")

  # f = getTaskFormula(.task)
  # browser()

  shapeletLib::shapelet_classification(method = .learner$par.set$pars$method$default,
                                       class.type = "binary", K = .learner$par.set$pars$K$default * length(d),
                                       L = .learner$par.set$pars$L$default* length(d), C = .learner$par.set$pars$C$default,
                                       data.train = z$datamatrix(unlist(d), ncol = length(d),  byrow = FALSE)[,-1],
                                       label.train =  ((as.numeric(d[,all.vars(f)[1]])-2)*2)+1,
                                       step = .learner$par.set$pars$step$default, max.iter = 100,
                                       init.method = .learner$par.set$pars$init.method$default)

}


#' @export
predictLearner.classif.rpart = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "prob", "class")
  predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

#' @export
getFeatureImportanceLearner.classif.rpart = function(.learner, .model, ...) {
  mod = getLearnerModel(.model)
  mod$variable.importance
}
