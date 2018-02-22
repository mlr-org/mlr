#' @export
makeRLearner.regr.mxff = function() {
  makeRLearnerRegr(
    cl = "regr.mxff",
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
      makeDiscreteLearnerParam(id = "act.out", default = "rmse",
        values = c("rmse", "mae")),
      makeLogicalLearnerParam(id = "conv.layer1", default = FALSE),
      makeLogicalLearnerParam(id = "conv.layer2", default = FALSE,
        requires = quote(layers > 1 && conv.layer1 == TRUE)),
      makeLogicalLearnerParam(id = "conv.layer3", default = FALSE,
        requires = quote(layers > 2 && conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.data.shape",
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.kernel1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.kernel2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.kernel3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.stride1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.stride2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.stride3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.dilate1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.dilate2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.dilate3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.pad1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.pad2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "conv.pad3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.kernel1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.kernel2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.kernel3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.stride1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.stride2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.stride3", lower = 1L, len = 2,
        requires = quote(conv.layer3 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.pad1", lower = 1L, len = 2,
        requires = quote(conv.layer1 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.pad2", lower = 1L, len = 2,
        requires = quote(conv.layer2 == TRUE)),
      makeIntegerVectorLearnerParam(id = "pool.pad3", lower = 1L, len = 2,
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
      makeNumericLearnerParam(id = "validation.ratio"),
      makeIntegerLearnerParam(id = "early.stop.badsteps", lower = 1),
      makeLogicalLearnerParam(id = "early.stop.maximize", default = TRUE),
      makeLogicalLearnerParam(id = "dropout.global", default = TRUE),
      makeNumericLearnerParam(id = "dropout.input", lower = 0, upper = 1 - 1e-7),
      makeNumericLearnerParam(id = "dropout.layer1", lower = 0, upper = 1 - 1e-7,
        requires = quote(dropout.global == FALSE)),
      makeNumericLearnerParam(id = "dropout.layer2", lower = 0, upper = 1 - 1e-7,
        requires = quote(dropout.global == FALSE)),
      makeNumericLearnerParam(id = "dropout.layer3", lower = 0, upper = 1 - 1e-7,
        requires = quote(dropout.global == FALSE)),
      makeDiscreteLearnerParam(id = "dropout.mode", default = "training", values = c("training", "always")),
      makeIntegerLearnerParam(id = "dropout.predict.repls", default = 20, lower = 2, requires = quote(dropout.mode  == "always")),
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
    properties = c("numerics", "se"),
    par.vals = list(learning.rate = 0.1, array.layout = "rowmajor", verbose = FALSE),
    name = "Feedforward Neural Network",
    short.name = "mxff",
    note = "Default of `learning.rate` set to `0.1`. Default of `array.layout` set to `'rowmajor'`.
    Default of `verbose` is set to `FALSE`. If `symbol` is specified, it will be passed to mxnet
    ignoring other architectural specifications. Default of `initializer` is set to NULL, which
    results in the default mxnet initializer being called when training a model. Number of output
    nodes is detected automatically. The upper bound for dropout is set to `1 - 1e-7` as in `mx.mlp`
    in the `mxnet` package. If `dropout.global` is `TRUE`, the same dropout rate `dropout.input`
    will be applied to the inputs and all the hidden layers. If `dropout.global` is `FALSE`,
    `dropout.input` will be applied to the inputs, and the different `dropout.layer` parameters to
    their respective layers. `dropout.mode` specifies if dropout should only be used in training or
    also for predictions, which allows stochastic predictions. This can be controlled for `se` with `dropout.predict.repls`.
    If `conv.layer1` is `FALSE`, the first layer is a `FullyConnected` layer and `num.layer1` gives
    the number of neurons. If `conv.layer1` is `TRUE`, then `num.layer1` gives the number of
    filters. In this case, `act1` is applied as an `Activation` layer afterwards (as is the case
    with a `FullyConnected` layer).
    This is the same for `conv.layer2` and `conv.layer3`. A `Convolution`
    layer cannot follow a `FullyConnected` layer. To stick with the example of the first layer,
    `conv.kernel1`, `conv.stride1`,
    `conv.dilate1` and `conv.pad1` correspond to the parameters
    of `mx.symbol.Convolution`. When a `Convolution` layer is constructed, a `Pooling` layer is
    constructed with it automatically. Again sticking to the example of the first layer,
    `pool.kernel1`, `pool.stride1`, `pool.pad1`
    and `pool.type1` correspond to the parameters in `mx.symbol.Pooling`.
    When convolution is used, `conv.data.shape` needs to be specified, which is a vector giving the
    dimensionality of the data (e.g. for MNIST `c(28, 28)` or `c(28, 28, 1)` and for CIFAR10
    `c(28, 28, 3)`). Furthermore, `array.layout` is set to `colmajor` if convolution is used, to
    enable compatability with `mxnet`. When using convolution,
    `mx.model.FeedForward.create` expects the array containing the data to have `4` dimensions.
    To allow for flexibility, `conv.data.shape` can have length `1` to `4`, the dimensions are
    taken in ascending order. For common cases, giving an `conv.data.shape` of length `2` or `3`is
    sufficient.
    `validation.ratio` gives the ratio of training data that will not
    be used for training but as validation data similar to the data provided in `eval.data`.
    If `eval.data` is specified, `validation.ratio` will be ignored. Note that `eval.data` is passed
    to `mx.model.FeedForward.create` unchanged to provide unconstrained usability of the underlying
    learner. In particular, this implies that `array.layout` is not adapted when using convolution,
    so `eval.data` needs to be provided in the right format.
    If `validation.ratio` is specified, it is sampled randomly using `R`'s `sample`.
    If `early.stop.badsteps` is specified and `epoch.end.callback` is not specified,
    early stopping will be used using `mx.callback.early.stop` as `epoch.end.callback` with the
    learner's `eval.metric`. In this case, `early.stop.badsteps` gives the number of `bad.steps` in
    `mx.callback.early.stop` and `early.stop.maximize` gives the `maximize` parameter in
    `mx.callback.early.stop`. Please note that when using `early.stop.badsteps`, `eval.metric` and
    either `eval.data` or `validation.ratio` should be specified.
    "
  )
}

#' @export
trainLearner.regr.mxff = function(.learner, .task, .subset, .weights = NULL,
  layers = 1L, num.layer1 = 1L, num.layer2 = 1L, num.layer3 = 1L,
  act1 = "tanh", act2 = "tanh", act3 = "tanh", act.out = "rmse",
  conv.data.shape = NULL, conv.layer1 = FALSE, conv.layer2 = FALSE, conv.layer3 = FALSE,
  conv.kernel1 = NULL, conv.kernel2 = NULL, conv.kernel3 = NULL,
  conv.stride1 = NULL, conv.stride2 = NULL, conv.stride3 = NULL,
  conv.dilate1 = NULL, conv.dilate2 = NULL, conv.dilate3 = NULL,
  conv.pad1 = NULL, conv.pad2 = NULL, conv.pad3 = NULL,
  pool.kernel1 = NULL, pool.kernel2 = NULL, pool.kernel3 = NULL,
  pool.stride1 = NULL, pool.stride2 = NULL, pool.stride3 = NULL,
  pool.pad1 = NULL, pool.pad2 = NULL, pool.pad3 = NULL,
  pool.type1 = "max", pool.type2 = "max", pool.type3 = "max",
  dropout.global = TRUE, dropout.input = NULL,
  dropout.layer1 = NULL, dropout.layer2 = NULL, dropout.layer3 = NULL,
  symbol = NULL, validation.ratio = NULL, eval.data = NULL,
  early.stop.badsteps = NULL, epoch.end.callback = NULL, early.stop.maximize = TRUE,
  array.layout = "rowmajor", dropout.mode = "training", dropout.predict.repls = 20, ...) {

  if (.learner$predict.type == "se" & dropout.mode != "always")
    stop("dropout.mode needs to be equal to 'always' for se prediction.")

  # transform data in correct format
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  y = as.numeric(d$target) - 1
  x = data.matrix(d$data)

  # construct validation data
  if (is.null(eval.data) & !is.null(validation.ratio)) {
    eval.data = list()
    rdesc = makeResampleDesc("Holdout", split = 1 - validation.ratio, stratify = TRUE)
    rinst = makeResampleInstance(rdesc, subsetTask(.task, subset = .subset))
    val.ind = rinst$test.inds[[1]]
    eval.data$label = y[val.ind]
    y = y[-val.ind]
    eval.data$data = x[val.ind, ]
    x = x[-val.ind, ]
  }

  # if convolution is used, prepare the data dimensionality
  if (conv.layer1) {
    l = length(.learner$par.vals$conv.data.shape)
    dims = switch(l,
      # for one-dimensional data
      c(.learner$par.vals$conv.data.shape, 1, 1, nrow(x)),
      # for two-dimensional data (e.g. MNIST)
      c(.learner$par.vals$conv.data.shape, 1, nrow(x)),
      # for three-dimensional data (e.g. images with color levels)
      c(.learner$par.vals$conv.data.shape, nrow(x)),
      # dimensionality of the data completely defined by user
      .learner$par.vals$conv.data.shape)
    x = array(aperm(x), dim = dims)
    # adapt array.layout for mx.model.FeedForward.create
    array.layout = "colmajor"
    # adapt validation data if necessary
    if (!is.null(validation.ratio)) {
      dims = switch(l,
        # for one-dimensional data
        c(.learner$par.vals$conv.data.shape, 1, 1, nrow(eval.data$data)),
        # for two-dimensional data (e.g. MNIST)
        c(.learner$par.vals$conv.data.shape, 1, nrow(eval.data$data)),
        # for three-dimensional data (e.g. images with color levels)
        c(.learner$par.vals$conv.data.shape, nrow(eval.data$data)),
        # dimensionality of the data completely defined by user
        .learner$par.vals$conv.data.shape)
      eval.data$data = array(aperm(eval.data$data), dim = dims)
    }
  }

  # early stopping
  if (is.null(epoch.end.callback) & !is.null(early.stop.badsteps)) {
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
    convs = c(conv.layer1, conv.layer2, conv.layer3)[1:layers]
    conv.kernels = list(conv.kernel1, conv.kernel2, conv.kernel3)[1:layers]
    conv.strides = list(conv.stride1, conv.stride2, conv.stride3)[1:layers]
    conv.dilates = list(conv.dilate1, conv.dilate2, conv.dilate3)[1:layers]
    conv.pads = list(conv.pad1, conv.pad2, conv.pad3)[1:layers]
    pool.kernels = list(pool.kernel1, pool.kernel2, pool.kernel3)[1:layers]
    pool.strides = list(pool.stride1, pool.stride2, pool.stride3)[1:layers]
    pool.pads = list(pool.pad1, pool.pad2, pool.pad3)[1:layers]
    pool.types = list(pool.type1, pool.type2, pool.type3)[1:layers]

    # add dropout if specified
    if (dropout.global == TRUE) {
      # construct a list of dropout rates
      if (is.null(dropout.input)) {
        dropout = NULL
      } else {
        dropout = as.list(rep(dropout.input, times = layers + 1))
      }
    } else {
      dropout = list(dropout.input, dropout.layer1, dropout.layer2, dropout.layer3)
    }

    if (!is.null(dropout[[1]])) {
      sym = mxnet::mx.symbol.Dropout(sym, p = dropout[[1]], mode = dropout.mode)
    }

    # construct hidden layers using symbols
    for (i in seq_len(layers)) {
      if (convs[i]) {
        # construct convolutional layer with pooling
        # prepare convolution inputs
        conv.inputs = list(data = sym, kernel = conv.kernels[[i]], stride = conv.strides[[i]],
          dilate = conv.dilates[[i]], pad = conv.pads[[i]], num_filter = nums[i])
        # construct convolutional layer with do.call to omit null values
        sym = do.call(mxnet::mx.symbol.Convolution, conv.inputs[!sapply(conv.inputs, is.null)])
        # add activation
        sym = mxnet::mx.symbol.Activation(sym, act_type = act[i])
        # prepare pooling inputs
        pool.inputs = list(data = sym, kernel = pool.kernels[[i]], pool.type = pool.types[[i]],
          stride = pool.strides[[i]], pad = pool.pads[[i]])
        # construct pooling layer with do.call to omit null values
        sym = do.call(mxnet::mx.symbol.Pooling, pool.inputs[!sapply(pool.inputs, is.null)])
      } else {
        # construct fully connected layer
        # if there has been a preceeding convolutional layer, the symbol shape needs to be adapted
        # check if this is the first layer
        if (i > 1) {
          # check if there has been a conlolutional layer
          if (convs[i - 1]) {
            sym = mxnet::mx.symbol.flatten(sym)
          }
        }
        sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = nums[i])
        sym = mxnet::mx.symbol.Activation(sym, act_type = act[i])
      }
      # add dropout if specified
      if (!is.null(dropout[[i + 1]])) {
        sym = mxnet::mx.symbol.Dropout(sym, p = dropout[[i + 1]], mode = dropout.mode)
      }
    }

    # construct output layer
    if (convs[layers]) {
      sym = mxnet::mx.symbol.flatten(sym)
    }
    sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = 1)
    out = switch(act.out,
      rmse = mxnet::mx.symbol.LinearRegressionOutput(sym),
      mae = mxnet::mx.symbol.MAERegressionOutput(sym),
      stop("Output activation not supported yet."))
  }

  # create model
  model = mxnet::mx.model.FeedForward.create(out, X = x, y = y, eval.data = eval.data,
    epoch.end.callback = epoch.end.callback, array.layout = array.layout, ...)
  return(model)
}

#' @export
predictLearner.regr.mxff = function(.learner, .model, .newdata, ...) {
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
  if (.learner$predict.type == "se") {
    p = replicate(.learner$par.vals$dropout.predict.repls, predict(.model$learner.model, X = x, array.layout = array.layout)[1, ])
    cbind(rowMeans(p), apply(p, 1, sd))
  } else {
    predict(.model$learner.model, X = x, array.layout = array.layout)[1, ]
  }

}

