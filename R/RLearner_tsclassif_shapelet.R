#' @export
makeRLearner.tsclassif.shapelet = function() {
  makeRLearnerClassif(
    cl = "tsclassif.shapelet",
    package = "shapeletLib",
    par.set = makeParamSet(
      #FIXME: how to insert params which is matrix/default = NULL ?
      makeDiscreteLearnerParam(id = "method", default = "hinge", values = list("hinge", "log")),
      makeNumericLearnerParam(id = "k", default = 0.02, lower = 0.001),
      makeNumericLearnerParam(id = "l", default = 0.2, lower = 0.001, upper = 1),
      makeNumericLearnerParam(id = "c.reg", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda"),
      makeDiscreteLearnerParam(id = "step", default = "pegasos", values = list("pegasos", "sqrt", "user")),
      makeNumericLearnerParam(id = "step.size"),
      makeIntegerLearnerParam(id = "max.iter", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "init", default = "kmeans", values = list("kmeans", "random", "user")),
      makeUntypedLearnerParam(id = "init.shapes"),
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
  shapeletLib::learnShapelets(data = z$data, label = as.factor(z$target), ...)
}

#' @export
predictLearner.tsclassif.shapelet = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd = as.matrix(.newdata)
  class.pred = shapeletLib::predictDataClass(model = m, data.test = nd)

  # FIXME: outcome of method is -1+1 -> how to recode to 1,2 and generalize it to other labels ?
  # class.pred[class.pred == -1] = 2
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  class.pred = as.factor(ifelse(class.pred > 0, levs[2L], levs[1L]))

  return(class.pred)
}


