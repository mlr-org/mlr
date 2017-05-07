#' @export
makeRLearner.classif.mxconv = function() {
  makeRLearnerClassif(
    cl = "classif.mxff",
    package = "mxnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "nodes1", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nodes2", lower = 0L,
        requires = quote(testIntegerish(nodes1, lower = 1))),
      makeIntegerLearnerParam(id = "nodes3", lower = 0L,
        requires = quote(testIntegerish(nodes2, lower = 1))),
      makeIntegerLearnerParam(id = "nodes_out", lower = 1L, default = 2),
      makeDiscreteLearnerParam(id = "act1", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu")),
      makeDiscreteLearnerParam(id = "act2", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(testIntegerish(nodes1, lower = 1) && testIntegerish(nodes2, lower = 1))),
      makeDiscreteLearnerParam(id = "act3", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(testIntegerish(nodes2, lower = 1) && testIntegerish(nodes3, lower = 1))),
      makeDiscreteLearnerParam(id = "act_out", default = "softmax",
        values = c("rmse", "softmax", "logistic")),
      makeNumericLearnerParam(id = "dropout", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "ctx", default = mx.ctx.default(), tunable = FALSE),
      makeIntegerLearnerParam(id = "begin.round", default = 1),
      makeIntegerLearnerParam(id = "num.round", default = 10),
      makeDiscreteLearnerParam(id = "optimizer", default = "sgd",
        values = c("sgd", "rmsprop", "adam", "adagrad", "adadelta")),
      makeUntypedLearnerParam(id = "initializer", default = mx.init.uniform(0.01)),
      makeUntypedLearnerParam(id = "eval.data", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval.metric", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "epoch.end.callback", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "batch.end.callback", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "array.batch.size", default = 128),
      makeDiscreteLearnerParam(id = "array.layout", default = "rowmajor",
        values = c("auto", "colmajor", "rowmajor"), tunable = FALSE),
      makeUntypedLearnerParam(id = "kvstore", default = "local", tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeUntypedLearnerParam(id = "arg.params", tunable = FALSE),
      makeUntypedLearnerParam(id = "aux.params", tunable = FALSE),
      makeNumericLearnerParam(id = "rho", default = 0.9, requires = quote(optimizer %in%
          c("adadelta"))),
      makeNumericLearnerParam(id = "epsilon",
        requires = quote(optimizer %in% c("adadelta", "adagrad", "adam"))),
      makeNumericLearnerParam(id = "wd", default = 0,
        requires = quote(optimizer %in% c("adadelta", "adagrad", "adam", "rmsprop", "sgd"))),
      makeNumericLearnerParam(id = "rescale.grad", default = 1,
        requires = quote(optimizer %in% c("adadelta", "adagrad", "adam", "rmsprop", "sgd"))),
      makeNumericLearnerParam(id = "clip_gradient",
        requires = quote(optimizer %in% c("adadelta", "adagrad", "adam", "rmsprop", "sgd"))),
      makeFunctionLearnerParam(id = "lr_scheduler",
        requires = quote(optimizer %in% c("adagrad", "adam", "rmsprop", "sgd"))),
      makeNumericLearnerParam(id = "learning.rate",
        requires = quote(optimizer %in% c("adagrad", "adam", "rmsprop", "sgd"))),
      makeNumericLearnerParam(id = "beta1", default = 0.9,
        requires = quote(optimizer %in% c("adam"))),
      makeNumericLearnerParam(id = "beta2", default = 0.999,
        requires = quote(optimizer %in% c("adam"))),
      makeNumericLearnerParam(id = "gamma1", default = 0.95,
        requires = quote(optimizer %in% c("rmsprop"))),
      makeNumericLearnerParam(id = "gamma2", default = 0.9,
        requires = quote(optimizer %in% c("rmsprop"))),
      makeNumericLearnerParam(id = "momentum", default = 0,
        requires = quote(optimizer %in% c("sgd")))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    par.vals = list(learning.rate = 0.1, array.layout = "rowmajor", verbose = FALSE),
    name = "mxff",
    note = "Default of `learning.rate` set to `0.1`. Default of `array.layout` set to `'rowmajor'`.
    Default of `verbose` is set to `FALSE`."
  )
}

#' @export
trainLearner.classif.mxconv = function(.learner, .task, .subset, .weights = NULL,
  nodes1 = 1, nodes2 = NULL, nodes3 = NULL, nodes_out = NULL,
  act1 = "tanh", act2 = NULL, act3 = NULL, act_out = "softmax", dropout = NULL, ...) {
  # transform data into correct format
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  y = as.numeric(d$target) - 1
  X = data.matrix(d$data)



  data <- mx.symbol.Variable('data')
  # first conv
  conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
  tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
  pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
    kernel=c(2,2), stride=c(2,2))
  # second conv
  conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
  tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
  pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
    kernel=c(2,2), stride=c(2,2))
  # first fullc
  flatten <- mx.symbol.Flatten(data=pool2)
  fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
  tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
  # second fullc
  fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
  # loss
  lenet <- mx.symbol.SoftmaxOutput(data=fc2)

 # create model
  model = mx.model.FeedForward.create(out, X = X, y = y, ...)

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
