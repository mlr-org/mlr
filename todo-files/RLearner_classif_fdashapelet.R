# To be added when Shapeletlib hits CRAN

#' @title Shapelet Model Learner.
#'
#' @description
#' Learner for Shapelet learning for classification.
#'
#' @export
makeRLearner.fdaclassif.shapelet = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.shapelet",
    package = "shapeletLib",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "method", default = "hinge", values = list("hinge", "log")),
      makeNumericLearnerParam(id = "k", default = 0.02, lower = 0.001),
      makeNumericLearnerParam(id = "l", default = 0.2, lower = 0.001, upper = 1),
      makeNumericLearnerParam(id = "c.reg", default = 1, lower = 0),
      makeNumericLearnerParam(id = "lambda"),
      makeDiscreteLearnerParam(id = "step", default = "pegasos", values = list("pegasos", "sqrt", "user")),
      makeNumericLearnerParam(id = "step.size", requires = quote(step == "user")),
      makeIntegerLearnerParam(id = "max.iter", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "init", default = "kmeans", values = list("kmeans", "random", "user")),
      makeUntypedLearnerParam(id = "init.shapes", requires = quote(init == "user")),
      makeLogicalLearnerParam(id = "auto.hinge", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "show.info", default = FALSE, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Shapelet classification",
    short.name = "shapelets"
  )
}

#' @export
trainLearner.fdaclassif.shapelet = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "-1+1")
  shapeletLib::learnShapeletModel(data = z$data, label = as.factor(z$target), ...)
}

#' @export
predictLearner.fdaclassif.shapelet = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  nd = as.matrix(.newdata)
  class.pred = predict(object = m, newdata = nd)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  class.pred = as.factor(ifelse(class.pred > 0, levs[2L], levs[1L]))
  return(class.pred)
}
