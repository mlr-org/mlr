#' @export
makeRLearner.classif.mxff = function() {
  makeRLearnerClassif(
    cl = "classif.mxff",
    package = "mxnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "layers", lower = 1L, upper = 3L, default = 1L),
      makeIntegerLearnerParam(id = "nodes1", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nodes2", lower = 1L, requires = quote(layers > 1)),
      makeIntegerLearnerParam(id = "nodes3", lower = 1L, requires = quote(layers > 2)),
      makeDiscreteLearnerParam(id = "act1", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu")),
      makeDiscreteLearnerParam(id = "act2", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(layers > 1)),
      makeDiscreteLearnerParam(id = "act3", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(layers > 2)),
      makeDiscreteLearnerParam(id = "act_out", default = "softmax",
        values = c("rmse", "softmax", "logistic")),
      # other hyperparameters
      makeNumericLearnerParam(id = "dropout", lower = 0, upper = 1 - 1e-7),
      makeUntypedLearnerParam(id = "ctx", default = mx.ctx.default(), tunable = FALSE),
      makeIntegerLearnerParam(id = "begin.round", default = 1L),
      makeIntegerLearnerParam(id = "num.round", default = 10L),
      makeDiscreteLearnerParam(id = "optimizer", default = "sgd",
        values = c("sgd", "rmsprop", "adam", "adagrad", "adadelta")),
      makeUntypedLearnerParam(id = "initializer", default = NULL),
      makeUntypedLearnerParam(id = "eval.data", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval.metric", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "epoch.end.callback", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "batch.end.callback", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "array.batch.size", default = 128L),
      makeDiscreteLearnerParam(id = "array.layout", default = "rowmajor",
        values = c("auto", "colmajor", "rowmajor"), tunable = FALSE),
      makeUntypedLearnerParam(id = "kvstore", default = "local", tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE),
      makeUntypedLearnerParam(id = "arg.params", tunable = FALSE),
      makeUntypedLearnerParam(id = "aux.params", tunable = FALSE),
      makeUntypedLearnerParam(id = "symbol", tunable = FALSE),
      # optimizer specific hyperhyperparameters
      makeNumericLearnerParam(id = "rho", default = 0.9, requires = quote(optimizer == "adadelta")),
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
      makeNumericLearnerParam(id = "beta1", default = 0.9, requires = quote(optimizer == "adam")),
      makeNumericLearnerParam(id = "beta2", default = 0.999, requires = quote(optimizer == "adam")),
      makeNumericLearnerParam(id = "gamma1", default = 0.95,
        requires = quote(optimizer == "rmsprop")),
      makeNumericLearnerParam(id = "gamma2", default = 0.9,
        requires = quote(optimizer == "rmsprop")),
      makeNumericLearnerParam(id = "momentum", default = 0, requires = quote(optimizer == "sgd"))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    par.vals = list(learning.rate = 0.1, array.layout = "rowmajor", verbose = FALSE),
    name = "Feedforward Neural Network",
    short.name = "mxff",
    note = "Default of `learning.rate` set to `0.1`. Default of `array.layout` set to `'rowmajor'`.
    Default of `verbose` is set to `FALSE`. If `symbol` is specified, it will be passed to mxnet
    ignoring other architectural specifications. Default of `initializer` is set to NULL, which
    results in the default mxnet initializer being called when training a model. Number of output
    nodes is detected automatically. The upper bound for dropout is set to `1 - 1e-7` as in `mx.mlp`
    in the `mxnet` package."
  )
}

#' @export
trainLearner.classif.mxff = function(.learner, .task, .subset, .weights = NULL,
  layers = 1L, nodes1 = 1L, nodes2 = NULL, nodes3 = NULL, nodes.out = NULL,
  act1 = "tanh", act2 = NULL, act3 = NULL, act_out = "softmax", dropout = NULL, symbol = NULL,
  ...) {
  # transform data in correct format
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  y = as.numeric(d$target) - 1
  X = data.matrix(d$data)

  # construct vectors with #nodes and activations
  if (!is.null(symbol)) {
    out = symbol
  } else {
    sym = mx.symbol.Variable("data")
    act = list(act1, act2, act3)[1:layers]
    nodes = list(nodes1, nodes2, nodes3)[1:layers]

    # construct hidden layers using symbols
    for (i in seq_len(layers)) {
      sym = mx.symbol.FullyConnected(sym, num_hidden = nodes[[i]])
      sym = mx.symbol.Activation(sym, act_type = act[[i]])
    }

    # add dropout if specified
    if (!is.null(dropout)) {
      sym = mx.symbol.Dropout(sym, p = dropout)
    }

    # construct output layer
    nodes.out = switch(act_out,
      softmax = nlevels(d$target),
      logistic = 1,
      stop("Output activation not supported yet."))
    sym = mx.symbol.FullyConnected(sym, num_hidden = nodes.out)
    out = switch(act_out,
      # rmse = mx.symbol.LinearRegressionOutput(sym),
      softmax = mx.symbol.SoftmaxOutput(sym),
      logistic = mx.symbol.LogisticRegressionOutput(sym),
      stop("Output activation not supported yet."))
  }

  # create model
  model = mx.model.FeedForward.create(out, X = X, y = y, ...)
  return(model)
}

#' @export
predictLearner.classif.mxff = function(.learner, .model, .newdata, ...) {
  x = data.matrix(.newdata)
  p = predict(.model$learner.model, X = x)
  if (.learner$predict.type == "response") {
    p = apply(p, 2, function(i) {
      w = which.max(i)
      return(ifelse(length(w > 0), w, NaN))
      })
    p = factor(p, exclude = NaN)
    levels(p) = .model$task.desc$class.levels
    return(p)
  }
  # if (.learner$predict.type == "prob")
}
