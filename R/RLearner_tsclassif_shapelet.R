#' @export
makeRLearner.tsclassif.shapelet = function() {
  makeRLearnerClassif(
    cl = "tsclassif.shapelet",
    package = "shapeletLib",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "K", lower = 1),
      makeIntegerLearnerParam(id = "L", lower = 1),
      makeIntegerLearnerParam(id = "max.iter", lower = 1)
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
  K = 3, L = 20, max.iter = 100L, method = "hinge", C = 1, step = "sqrt", init.method = "kmeans", ...) {
  #FIXME: args above need defaults in shapeletLib! we use stupid vals here now...!

  z = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "-1+1")
  # FIXME: this is all bullshit!
  class.type = "binary"

  shapelet_classification(data.train = as.matrix(z$data), label.train = z$target,
    class.type = class.type, method = method, K = K, L = L, max.iter = max.iter,
    C = C, step = step, init.method = init.method, ...)
}

#' @export
predictLearner.tsclassif.shapelet = function(.learner, .model, .newdata, ...) {
  # FIXME: we need a better predict funtion in shapeletLib!!!
  m = .model$learner.model
  iter = m$ConvIt
  nd = as.matrix(.newdata)
  print(str(nd))
  ts.trafo = shapeletLib::make_transform(TS.data = nd, model = m, iter = iter)
  print(str(ts.trafo))

  if (m$method == "log") {
    y.pred = apply(ts.trafo, 1, function(x)
      pred_class_log(W = m$WOld[[iter]], biasW = m$biasWOld[[iter]], Mi = x))
    class.pred = get_class_log(pred = y.pred)
  } else {
    class.pred = apply(ts.trafo, 1, function(x)
      pred_class_hinge(W = m$WOld[[iter]], biasW = m$biasWOld[[iter]], Mi = x))
  }
  print(str(class.pred))
  return(class.pred)
}


