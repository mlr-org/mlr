#' @export
makeRLearner.tsclassif.shapelet = function() {
  makeRLearnerClassif(
    cl = "tsclassif.shapelet",
    package = "shapeletLib",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "K", default = 0.02, lower = 0.01),
      makeNumericLearnerParam(id = "L", default = 0.2, lower = 0.01),
      makeIntegerLearnerParam(id = "max.iter", lower = 1L)
      # makeNumericLearnerParam(id = "distance", default = 2, lower = 0),
      # makeDiscreteLearnerParam(id = "kernel", default = "triangular",
        # values = list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
      # makeLogicalLearnerParam(id = "scale", default = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Shapelet classification",
    short.name = "shapelets"
  )
}

#' @export
trainLearner.tsclassif.shapelet = function(.learner, .task, .subset, .weights = NULL,
  method = "hinge", ...) {

  z = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "-1+1")

  shapeletLib::learnShapelets(data.train = as.matrix(z$data), label.train = z$target,
    method = method,  ...)
}

#' @export
predictLearner.tsclassif.shapelet = function(.learner, .model, .newdata, ...) {
  # FIXME: we need a better predict funtion in shapeletLib!!!
  m = .model$learner.model
  nd = as.matrix(.newdata)
  class.pred = shapetlLib::predictDataClass(model = m, data.test = nd)
 # browser()
  # iter = m$ConvIt
  # nd = as.matrix(.newdata)
  # print(str(nd))
  # ts.trafo = shapeletLib::make_transform(TS.data = nd, model = m, iter = iter)
  # print(str(ts.trafo))
  #
  # if (m$method == "log") {
  #   y.pred = apply(ts.trafo, 1, function(x)
  #     pred_class_log(W = m$WOld[[iter]], biasW = m$biasWOld[[iter]], Mi = x))
  #   class.pred = get_class_log(pred = y.pred)
  # } else {
  #   class.pred = apply(ts.trafo, 1, function(x)
  #     pred_class_hinge(W = m$WOld[[iter]], biasW = m$biasWOld[[iter]], Mi = x))
  # }
  class.pred.old = class.pred
  class.pred[class.pred == -1] = 2

  class.pred = as.factor(class.pred)
  return(class.pred)
}


