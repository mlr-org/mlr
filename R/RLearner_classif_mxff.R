#' @export
makeRLearner.classif.mxff = function() {
  makeRLearnerClassif(
    cl = "classif.mxff",
    package = "mxnet",
    par.set = makeParamSet(
      # architectural hyperparameters
      makeIntegerLearnerParam(id = "layers", lower = 1L, upper = 3L, default = 1L),
      makeIntegerLearnerParam(id = "num.layer1", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "num.layer2", lower = 1L, default = 1L,
        requires = quote(layers > 1)),
      makeIntegerLearnerParam(id = "num.layer3", lower = 1L, default = 1L,
        requires = quote(layers > 2)),
      makeDiscreteLearnerParam(id = "act1", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu")),
      makeDiscreteLearnerParam(id = "act2", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(layers > 1)),
      makeDiscreteLearnerParam(id = "act3", default = "tanh",
        values = c("tanh", "relu", "sigmoid", "softrelu"),
        requires = quote(layers > 2)),
      makeDiscreteLearnerParam(id = "act.out", default = "softmax",
        values = c("rmse", "softmax", "logistic")),
      makeLogicalLearnerParam(id = "conv.layer1", default = FALSE),
      makeLogicalLearnerParam(id = "conv.layer2", default = FALSE,
        requires = quote(layers > 1 && conv.layer1 == TRUE)),
      makeLogicalLearnerParam(id = "conv.layer3", default = FALSE,
        requires = quote(layers > 2 && conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.data.shape",
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.kernel32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.stride32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.dilate32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "conv.pad32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.kernel32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.stride32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad11", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad12", lower = 1L,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad21", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad22", lower = 1L,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad31", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerLearnerParam(id = "pool.pad32", lower = 1L,
        requires = quote(conv.layer3 == TRUE)),
      makeDiscreteLearnerParam(id = "pool.type1", default = "max",
        values = c("max", "avg", "sum"),
        requires = quote(conv.layer1 == TRUE)),
      makeDiscreteLearnerParam(id = "pool.type2", default = "max",
        values = c("max", "avg", "sum"),
        requires = quote(conv.layer2 == TRUE)),
      makeDiscreteLearnerParam(id = "pool.type3", default = "max",
        values = c("max", "avg", "sum"),
        requires = quote(conv.layer3 == TRUE)),
      # other hyperparameters
      makeNumericLearnerParam(id = "validation.set"),
      makeIntegerLearnerParam(id = "early.stop.badsteps", lower = 1),
      makeLogicalLearnerParam(id = "early.stop.maximize", default = TRUE),
      makeNumericVectorLearnerParam(id = "dropout", lower = 0, upper = 1 - 1e-7),
      makeUntypedLearnerParam(id = "ctx", default = mxnet::mx.ctx.default(), tunable = FALSE),
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
    properties = c("twoclass", "multiclass", "numerics", "prob"),
    par.vals = list(learning.rate = 0.1, array.layout = "rowmajor", verbose = FALSE),
    name = "Feedforward Neural Network",
    short.name = "mxff",
    note = "Default of `learning.rate` set to `0.1`. Default of `array.layout` set to `'rowmajor'`.
    Default of `verbose` is set to `FALSE`. If `symbol` is specified, it will be passed to mxnet
    ignoring other architectural specifications. Default of `initializer` is set to NULL, which
    results in the default mxnet initializer being called when training a model. Number of output
    nodes is detected automatically. The upper bound for dropout is set to `1 - 1e-7` as in `mx.mlp`
    in the `mxnet` package. Dropout should be a numeric vector of length `1`, `2`, or `layers + 1`.
    If `length(dropout)` is `1`, the same dropout rate will be applied to the inputs and all the
    hidden layers. If `length(dropout)` is `2`, the first rate will be applied to the inputs, the
    second to all the hidden layers. If `length(dropout)` is `layers + 1` then the input and every
    hidden layers will be assigned an individual dropout rate.
    If `conv.layer1` is `FALSE`, the first layer is a `FullyConnected` layer and `num.layer1` gives
    the number of neurons. If `conv.layer1` is `TRUE`, then `num.layer1` gives the number of
    filters. In this case, `act1` is applied as an `Activation` layer afterwards (as is the case
    with a `FullyConnected` layer).
    This is the same for `conv.layer2` and `conv.layer3`. A `Convolution`
    layer cannot follow a `FullyConnected` layer. To stick with the example of the first layer,
    `c(conv.kernel11, conv.kernel12)`, `c(conv.stride11, conv.stride12)`,
    `c(conv.dilate11, conv.dilate12)` and `c(conv.pad11, conv.pad12)` correspond to the parameters
    of `mx.symbol.Convolution`. When a `Convolution` layer is constructed, a `Pooling` layer is
    constructed with it automatically. Again sticking to the example of the first layer,
    `c(pool.kernel11, pool.kernel12)`, `c(pool.stride11, pool.stride12)`, `c(pool.pad11, pool.pad12)`
    and `c(pool.type11, pool.type12)` correspond to the parameters in `mx.symbol.Pooling`.
    If only the first parameter of a pair is defined, the second parameter is set to the same value,
    e.g. if `conv.kernel11` is defined and `conv.kernel12` is undefined, `conv.kernel12` is set to
    the value of `conv.kernel11`.
    When convolution is used, `conv.data.shape` needs to be specified, which is a vector giving the
    dimensionality of the data (e.g. for MNIST `c(28, 28)`). Furthermore, `array.layout` is set to
    `colmajor` if convolution is used, to enable compatability with `mxnet`. When using convolution,
    `mx.model.FeedForward.create` expects the array containing the data to have `4` dimensions.
    To allow for flexibility, `conv.data.shape` can have length `1` to `4`, the dimensions are
    taken in ascending order. For common cases, giving an `conv.data.shape` of length `2` is
    sufficient.
    `validation.set` gives the ratio of training data that will not
    be used for training but as validation data similar to the data provided in `eval.data`.
    If `eval.data` is specified, `validation.set` will be ignored. Note that `eval.data` is passed
    to `mx.model.FeedForward.create` unchanged to provide unconstrained usability of the underlying
    learner. In particular, this implies that `array.layout` is not adapted when using convolution,
    so `eval.data` needs to be provided in the right format.
    If `validation.set` is specified, it is sampled randomly using `R`'s `sample`.
    If `early.stop.badsteps` is specified and `epoch.end.callback` is not specified,
    early stopping will be used using `mx.callback.early.stop` as `epoch.end.callback` with the
    learner's `eval.metric`. In this case, `early.stop.badsteps` gives the number of `bad.steps` in
    `mx.callback.early.stop` and `early.stop.maximize` gives the `maximize` parameter in
    `mx.callback.early.stop`. Please note that when using `early.stop.badsteps`, `eval.metric` and
    either `eval.data` or `validation.set` should be specified.
    "
  )
}

#' @export
trainLearner.classif.mxff = function(.learner, .task, .subset, .weights = NULL,
  layers = 1L, num.layer1 = 1L, num.layer2 = 1L, num.layer3 = 1L,
  act1 = "tanh", act2 = "tanh", act3 = "tanh", act.out = "softmax",
  conv.data.shape = NULL, conv.layer1 = FALSE, conv.layer2 = FALSE, conv.layer3 = FALSE,
  conv.kernel11 = NULL, conv.kernel21 = NULL, conv.kernel31 = NULL,
  conv.kernel12 = NULL, conv.kernel22 = NULL, conv.kernel32 = NULL,
  conv.stride11 = NULL, conv.stride21 = NULL, conv.stride31 = NULL,
  conv.stride12 = NULL, conv.stride22 = NULL, conv.stride32 = NULL,
  conv.dilate11 = NULL, conv.dilate21 = NULL, conv.dilate31 = NULL,
  conv.dilate12 = NULL, conv.dilate22 = NULL, conv.dilate32 = NULL,
  conv.pad11 = NULL, conv.pad21 = NULL, conv.pad31 = NULL,
  conv.pad12 = NULL, conv.pad22 = NULL, conv.pad32 = NULL,
  pool.kernel11 = NULL, pool.kernel21 = NULL, pool.kernel31 = NULL,
  pool.kernel12 = NULL, pool.kernel22 = NULL, pool.kernel32 = NULL,
  pool.stride11 = NULL, pool.stride21 = NULL, pool.stride31 = NULL,
  pool.stride12 = NULL, pool.stride22 = NULL, pool.stride32 = NULL,
  pool.pad11 = NULL, pool.pad21 = NULL, pool.pad31 = NULL,
  pool.pad12 = NULL, pool.pad22 = NULL, pool.pad32 = NULL,
  pool.type1 = "max", pool.type2 = "max", pool.type3 = "max",
  dropout = NULL, symbol = NULL, validation.set = NULL, eval.data = NULL, early.stop.badsteps = NULL,
  epoch.end.callback = NULL, early.stop.maximize = TRUE, array.layout = "rowmajor", ...) {
  # transform data in correct format
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  y = as.numeric(d$target) - 1
  x = data.matrix(d$data)

  # construct validation data
  if (is.null(eval.data) & !is.null(validation.set)) {
    eval.data = list()
    n = dim(x)[1]
    val.ind = sample(n, floor(n * validation.set))
    eval.data$label = y[val.ind]
    y = y[-val.ind]
    eval.data$data = x[val.ind,]
    x = x[-val.ind,]
  }

  # if convolution is used, prepare the data dimensionality
  if (conv.layer1) {
    l = length(.learner$par.vals$conv.data.shape)
    dims = switch(l,
      c(.learner$par.vals$conv.data.shape, 1, 1, nrow(x)),
      c(.learner$par.vals$conv.data.shape, 1, nrow(x)),
      c(.learner$par.vals$conv.data.shape, nrow(x)),
      .learner$par.vals$conv.data.shape)
    x = array(aperm(x), dim = dims)
    # adapt array.layout for mx.model.FeedForward.create
    array.layout = "colmajor"
    # adapt validation data if necessary
    if (!is.null(validation.set)) {
      dims = switch(l,
        c(.learner$par.vals$conv.data.shape, 1, 1, nrow(eval.data$data)),
        c(.learner$par.vals$conv.data.shape, 1, nrow(eval.data$data)),
        c(.learner$par.vals$conv.data.shape, nrow(eval.data$data)),
        .learner$par.vals$conv.data.shape)
      eval.data$data = array(aperm(eval.data$data), dim = dims)
    }
  }

  # early stopping
  if (is.null(epoch.end.callback) & is.numeric(early.stop.badsteps)) {
    epoch.end.callback = mxnet::mx.callback.early.stop(bad.steps = early.stop.badsteps,
      maximize = early.stop.maximize)
  }

  # construct vectors with #nodes and activations
  if (!is.null(symbol)) {
    out = symbol
  } else {
    sym = mxnet::mx.symbol.Variable("data")
    act = c(act1, act2, act3)[1:layers]
    nums = c(num.layer1, num.layer2, num.layer3)[1:layers]
    convs = c(conv.layer1, conv.layer2, conv.layer3, FALSE)[1:layers]
    # define function to set e.g. conv.kernel12 = conv.kernel11 if conv.kernel12 == NULL
    fillVec = function(vec) {
      if (length(vec) == 1) {
        return(rep(vec, 2))
      } else {
        return(vec)
      }
    }
    # if layers is bigger than 3, NULL values are appended to the lists automatically
    conv.kernels = lapply(list(c(conv.kernel11, conv.kernel12), c(conv.kernel21, conv.kernel22),
      c(conv.kernel31, conv.kernel32))[1:layers], fillVec)
    conv.strides = lapply(list(c(conv.stride11, conv.stride12), c(conv.stride21, conv.stride22),
      c(conv.stride31, conv.stride32))[1:layers], fillVec)
    conv.dilates = lapply(list(c(conv.dilate11, conv.dilate12), c(conv.dilate21, conv.dilate22),
      c(conv.dilate31, conv.dilate32))[1:layers], fillVec)
    conv.pads = lapply(list(c(conv.pad11, conv.pad12), c(conv.pad21, conv.pad22),
      c(conv.pad31, conv.pad32))[1:layers], fillVec)
    pool.kernels = lapply(list(c(pool.kernel11, pool.kernel12), c(pool.kernel21, pool.kernel22),
      c(pool.kernel31, pool.kernel32))[1:layers], fillVec)
    pool.strides = lapply(list(c(pool.stride11, pool.stride12), c(pool.stride21, pool.stride22),
      c(pool.stride31, pool.stride32))[1:layers], fillVec)
    pool.pads = lapply(list(c(pool.pad11, pool.pad12), c(pool.pad21, pool.pad22),
      c(pool.pad31, pool.pad32))[1:layers], fillVec)
    pool.types = list(pool.type1, pool.type2, pool.type3)[1:layers]

    # add dropout if specified
    if (!is.null(dropout)) {
      # construct a vector of dropout rates
      if (length(dropout) == 1) {
        dropout = rep(dropout, times = layers + 1)
      } else if (length(dropout) == 2) {
        dropout = c(dropout[1], rep(dropout[2], times = layers))
      } else if (length(dropout) != (layers + 1)) {
        stop("Length of dropout should be 1, 2 or number of layers + 1!")
      }

      sym = mxnet::mx.symbol.Dropout(sym, p = dropout[1])
    }

    # construct hidden layers using symbols
    for (i in seq_len(layers)) {
      if (convs[i]) {
        # construct convolutional layer with pooling
        conv.inputs = list(data = sym, kernel = conv.kernels[[i]], stride = conv.strides[[i]],
          dilate = conv.dilates[[i]], pad = conv.pads[[i]], num_filter = nums[i])
        sym = do.call(mxnet::mx.symbol.Convolution, conv.inputs[!sapply(conv.inputs, is.null)])
        sym = mxnet::mx.symbol.Activation(sym, act_type = act[i])
        pool.inputs = list(data = sym, kernel = pool.kernels[[i]], pool.type = pool.types[[i]],
          stride = pool.strides[[i]], pad = pool.pads[[i]])
        sym = do.call(mxnet::mx.symbol.Pooling, pool.inputs[!sapply(pool.inputs, is.null)])
      } else {
        # construct fully connected layer
        if (i > 1) {
          if (convs[i - 1]) {
            sym = mxnet::mx.symbol.flatten(sym)
          }
        }
        sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = nums[i])
        sym = mxnet::mx.symbol.Activation(sym, act_type = act[i])
      }
      # add dropout if specified
      if (!is.null(dropout)) {
        sym = mxnet::mx.symbol.Dropout(sym, p = dropout[i + 1])
      }
    }

    # construct output layer
    if (convs[layers]) {
      sym = mxnet::mx.symbol.flatten(sym)
    }
    nodes.out = switch(act.out,
      softmax = nlevels(d$target),
      logistic = 1,
      stop("Output activation not supported yet."))
    sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = nodes.out)
    out = switch(act.out,
      # rmse = mxnet::mx.symbol.LinearRegressionOutput(sym),
      softmax = mxnet::mx.symbol.SoftmaxOutput(sym),
      logistic = mxnet::mx.symbol.LogisticRegressionOutput(sym),
      stop("Output activation not supported yet."))
  }

  # create model
  model = mxnet::mx.model.FeedForward.create(out, X = x, y = y, eval.data = eval.data,
    epoch.end.callback = epoch.end.callback, array.layout = array.layout, ...)
  return(model)
}

#' @export
predictLearner.classif.mxff = function(.learner, .model, .newdata, ...) {
  x = data.matrix(.newdata)
  array.layout = .model$learner$par.vals$array.layout
  conv.layer1 = ifelse(is.null(.learner$par.vals$conv.layer1),
    .learner$par.set$pars$conv.layer1$default, .learner$par.vals$conv.layer1)
  if (conv.layer1) {
    l = length(.learner$par.vals$conv.data.shape)
    dims = switch(l,
      c(.learner$par.vals$conv.data.shape, 1, 1, nrow(x)),
      c(.learner$par.vals$conv.data.shape, 1, nrow(x)),
      c(.learner$par.vals$conv.data.shape, nrow(x)),
      .learner$par.vals$conv.data.shape)
    x = array(aperm(x), dim = dims)
    array.layout = "colmajor"
  }
  p = predict(.model$learner.model, X = x, array.layout = array.layout)
  if (.learner$predict.type == "response") {
    p = apply(p, 2, function(i) {
      w = which.max(i)
      return(ifelse(length(w > 0), w, NaN))
    })
    p = factor(p, exclude = NaN)
    levels(p) = .model$task.desc$class.levels
    return(p)
  }
  if (.learner$predict.type == "prob") {
    p = t(p)
    colnames(p) = .model$task.desc$class.levels
    return(p)
  }
}

