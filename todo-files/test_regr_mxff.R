context("regr_mxff")

test_that("regr_mxff", {
  requirePackagesOrSkip("mxnet", default.method = "load")
  # tests for FeedForward networks with only FullyConnected layers using mx.mlp
  parset.list.mxnet = list(
    list(hidden_node = c(20, 20), activation = c("sigmoid", "relu"),
      learning.rate = 0.2, array.layout = "rowmajor", num.round = 100)#,
    # list(hidden_node = c(10, 6), activation = c("sigmoid", "relu"),
    #  learning.rate = 0.2, dropout = 0.5, array.layout = "rowmajor", num.round = 10)
  )
  parset.list.mlr = list(
    list(layers = 2, num.layer1 = 20, num.layer2 = 20, act1 = "sigmoid",
      act2 = "relu", learning.rate = 0.2, num.round = 100)#,
    # list(layers = 2, num.layer1 = 10, num.layer2 = 6, act1 = "sigmoid",
    #  act2 = "relu", learning.rate = 0.2, dropout.global = FALSE, dropout.layer1 = 0.5,
    # dropout.layer2 = 0.5, dropout.mode = "training", num.round = 10)
  )

  old.predicts.list = list()

  set.seed(getOption("mlr.debug.seed"))
  for (i in seq_along(parset.list.mxnet)) {
    x = data.matrix(regr.num.train[, -ncol(regr.num.train)])
    y = as.numeric(regr.num.train[, ncol(regr.num.train)]) - 1
#    pars = c(parset.list.mxnet[[i]], list(data = x, label = y), out_node = 1)
#    m = do.call(mxnet::mx.mlp, pars)
    sym = mxnet::mx.symbol.Variable("data")
    sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = parset.list.mxnet[[i]]$hidden_node[1])
    sym = mxnet::mx.symbol.Activation(sym, act_type = parset.list.mxnet[[i]]$activation[1])
    sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = parset.list.mxnet[[i]]$hidden_node[2])
    sym = mxnet::mx.symbol.Activation(sym, act_type = parset.list.mxnet[[i]]$activation[2])
    sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = 1)
    out = mxnet::mx.symbol.LinearRegressionOutput(sym)
    m = mxnet::mx.model.FeedForward.create(out, X = x, y = y, learning.rate = parset.list.mxnet[[i]]$learning.rate,
      num.round = parset.list.mxnet[[i]]$num.round, array.layout = parset.list.mxnet[[i]]$array.layout)
    y.pred = predict(m, data.matrix(regr.num.test[, -ncol(regr.num.test)]), array.layout = "rowmajor")[1, ]
    old.predicts.list[[i]] = y.pred
  }

  set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("regr.mxff", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list.mlr)
})
