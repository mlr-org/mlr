#' @export
makeRLearner.tsclassif.shapelet = function() {
  makeRLearnerClassif(
    cl = "tsclassif.shapelet",
    package = "shapeletLib",
    par.set = makeParamSet(
      #FIXME: how to insert params which is matrix/default = NULL ?
      makeDiscreteLearnerParam(id = "method", default ="hinge", values = list("hinge", "log")),
      makeNumericLearnerParam(id = "K", default = 0.02, lower = 0.001),
      makeNumericLearnerParam(id = "L", default = 0.2, lower = 0.001, upper = 1),
      makeNumericLearnerParam(id = "C", default = 1, lower = 0),
      makeUntypedLearnerParam(id = "step", default = "pegasos"),
      makeIntegerLearnerParam(id = "max.iter", lower = 1L),
      makeDiscreteLearnerParam(id = "init", default = "kmeans", values = list("kmeans", "random", "user")),
      makeLogicalLearnerParam(id = "auto.hinge", default = FALSE),
      makeLogicalLearnerParam(id = "show.info", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Shapelet classification",
    short.name = "shapelets"
  )
}

#' @export
trainLearner.tsclassif.shapelet = function(.learner, .task, .subset, .weights = NULL, ...) {

  z = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "-1+1")
  shapeletLib::learnShapelets(data.train = as.matrix(z$data), label.train = z$target, ...)
}

#' @export
predictLearner.tsclassif.shapelet = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd = as.matrix(.newdata)
  class.pred = shapeletLib::predictDataClass(model = m, data.test = nd)

  # FIXME: outcome of method is -1+1 -> how to recode to 1,2 and generalize it to other labels ?
  class.pred[class.pred == -1] = 2

  class.pred = as.factor(class.pred)
  return(class.pred)
}


