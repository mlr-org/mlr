context("classif_mxff")

test_that("classif_mxff", {
  requirePackagesOrSkip("mxnet", default.method = "load")
  parset.list.mxnet = list(
    list(out_node = 2, hidden_node = 1, learning.rate = 0.1),
    list(out_node = 2, hidden_node = c(10, 6), activation = c("sigmoid", "relu"),
      learning.rate = 0.2),
    list(out_node = 2, hidden_node = c(10, 6), activation = c("sigmoid", "relu"),
      learning.rate = 0.2, dropout = 0.5)
  )
  parset.list.mlr = list(
    list(),
    list(layers = 2, nodes1 = 10, nodes2 = 6, act1 = "sigmoid",
      act2 = "relu", learning.rate = 0.2),
    list(layers = 2, nodes1 = 10, nodes2 = 6, act1 = "sigmoid",
      act2 = "relu", learning.rate = 0.2, dropout = 0.5)
  )


  # binaryclass tests
  old.predicts.list = list()

  set.seed(getOption("mlr.debug.seed"))
  for (i in seq_along(parset.list.mxnet)) {
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = as.numeric(binaryclass.train[, ncol(binaryclass.train)]) - 1
    pars = c(parset.list.mxnet[[i]], list(data = x, label = y))
    m = do.call(mxnet::mx.mlp, pars)
    probs = predict(m, data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    p = apply(probs, 2, which.max)
    # avoid error when only one class is predicted
    p = as.factor(p, exclude = NaN)
    levels(p) = binaryclass.class.levs
    old.predicts.list[[i]] = p
  }

  set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("classif.mxff", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list.mlr)

  # multiclass tests
  old.predicts.list = list()

  set.seed(getOption("mlr.debug.seed"))
  for (i in seq_along(parset.list.mxnet)) {
    x = data.matrix(multiclass.train[, -ncol(multiclass.train)])
    y = as.numeric(multiclass.train[, ncol(multiclass.train)]) - 1
    y = y / max(y)
    pars = c(parset.list.mxnet[[i]], list(data = x, label = y))
    m = do.call(mxnet::mx.mlp, pars)
    probs = predict(m, data.matrix(multiclass.test[, -ncol(multiclass.test)]))
    p = apply(probs, 2, which.max)
    # avoid error when only one class is predicted
    p = as.factor(p, exclude = NaN)
    levels(p) = levels(multiclass.df[[multiclass.class.col]])
    old.predicts.list[[i]] = p
  }

  set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("classif.mxff", multiclass.df, multiclass.target, multiclass.train.inds,
    old.predicts.list, parset.list.mlr)
})
