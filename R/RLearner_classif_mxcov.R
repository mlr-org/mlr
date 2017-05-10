#' @export
makeRLearner.classif.mxconv = function() {
  makeRLearnerClassif(
    cl = "classif.mxff",
    package = "mxnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nodes1", lower = 1L, default = 1L)
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    par.vals = list(learning.rate = 0.1, array.layout = "rowmajor", verbose = FALSE),
    name = "mxff",
    note = "Default of `learning.rate` set to `0.1`. Default of `array.layout` set to `'rowmajor'`.
    Default of `verbose` is set to `FALSE`."
  )
}

#' @export
trainLearner.classif.mxconv = function(.learner, .task, .subset, .weights = NULL) {
  # transform data into correct format
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  y = as.numeric(d$target) - 1
  X = data.matrix(d$data)

  train.array <- train.x
  dim(train.array) <- c(201, 8, 1, ncol(train.x))

  device.cpu <- mx.cpu()
  data <- mx.symbol.Variable('data')
  # first conv
  conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,2), num_filter=2)
  tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
  pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
    kernel=c(2,1), stride=c(2,1))
  # second conv
  conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,2), num_filter=2)
  tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
  pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
    kernel=c(2,2), stride=c(2,1))
  # first fullc
  flatten <- mx.symbol.Flatten(data=pool2)
  fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
  tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
  # second fullc
  fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=2)  # two class classification
  # loss
  lenet <- mx.symbol.SoftmaxOutput(data=fc2)

  # create model

  model = mx.model.FeedForward.create(lenet, X= train.array, y=train.y,
    ctx=device.cpu, num.round=3, array.batch.size=100,
    learning.rate=0.05, momentum=0.9, wd=0.00001,
    eval.metric=mx.metric.accuracy,
    epoch.end.callback=mx.callback.log.train.metric(100))

  return(model)
}

#' @export
predictLearner.classif.conv = function(.learner, .model, .newdata, ...) {
  x = data.matrix(.newdata)
  p = predict(.model$learner.model, X = x)
  if (.learner$predict.type == "response") {
    p = apply(p, 2, function(i) {
      w = which.max(i)
      return(ifelse(length(w > 0), w, NaN))})
    p = factor(p, exclude = c(NaN))
    levels(p) = .model$task.desc$class.levels
    p
  }
}
