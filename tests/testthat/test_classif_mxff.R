context("classif_mxff")

test_that("classif_mxff", {
  requirePackagesOrSkip("mxnet", default.method = "load")
  # tests for FeedForward networks with only FullyConnected layers using mx.mlp
  parset.list.mxnet = list(
    list(hidden_node = c(10, 6), activation = c("sigmoid", "relu"),
      learning.rate = 0.2),
    list(hidden_node = c(10, 6), activation = c("sigmoid", "relu"),
      learning.rate = 0.2, dropout = 0.5)
  )
  parset.list.mlr = list(
    list(layers = 2, num.layer1 = 10, num.layer2 = 6, act1 = "sigmoid",
      act2 = "relu", learning.rate = 0.2),
    list(layers = 2, num.layer1 = 10, num.layer2 = 6, act1 = "sigmoid",
      act2 = "relu", learning.rate = 0.2, dropout = 0.5)
  )

  # test for Convolution
  parset.list.conv = list(
    list(num.filter = 1, kernel = c(1, 1), pool.type = "max", learning.rate = 0.1)
  )
  parset.list.mlr.conv = list(
    list(layers = 1, conv.layer1 = TRUE, num.layer1 = 1, conv.data.shape = c(2,2),
      conv.kernel11 = 1, conv.kernel12 = 1, pool.kernel11 = 1, pool.kernel12 = 1, pool.type1 = "max"
    )
  )

  # mxnet has its own internal random number generators so setting seeds in R does not work
  # therefore we need to change the testProbParsets function to account for some
  # tolerated difference in the probablities
  testProbParsetsWithTol = function(t.name, df, target, train.inds, old.probs.list, parset.list,
    tol = 1e-04) {
    inds = train.inds
    train = df[inds, ]
    test = df[-inds, ]

    for (i in seq_along(parset.list)) {
      parset = parset.list[[i]]
      old.probs = old.probs.list[[i]]
      testProbWithTol(t.name, df, target, train.inds, old.probs, parset, tol = tol)
    }
  }

  testProbWithTol = function(t.name, df, target, train.inds, old.probs, parset = list(),
    tol = 1e-04) {
    inds = train.inds
    train = df[inds, ]
    test = df[-inds, ]

    if (length(target) == 1) {
      task = makeClassifTask(data = df, target = target)
    } else {
      task = makeMultilabelTask(data = df, target = target)
    }
    lrn = do.call("makeLearner", c(t.name, parset, predict.type = "prob"))
    m = try(train(lrn, task, subset = inds))

    if (inherits(m, "FailureModel")) {
      expect_is(old.predicts, "try-error")
    } else{
      cp = predict(m, newdata = test)
      # dont need names for num vector, 2 classes
      if (is.numeric(old.probs))
        names(old.probs) = NULL
      else
        old.probs = as.matrix(old.probs)

      p = getPredictionProbabilities(cp)
      if (is.data.frame(p))
        p = as.matrix(p)
      # we change names a bit so dont check them
      colnames(p) = colnames(old.probs) = NULL
      rownames(p) = rownames(old.probs) = NULL
      class(old.probs) = NULL
      expect_equal(p, old.probs, tolerance = tol)
    }
  }

  # binaryclass tests
  old.predicts.list = list()
  old.probs.list = list()

  set.seed(getOption("mlr.debug.seed"))
  for (i in seq_along(parset.list.mxnet)) {
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = as.numeric(binaryclass.train[, ncol(binaryclass.train)]) - 1
    pars = c(parset.list.mxnet[[i]], list(data = x, label = y), out_node = 2)
    m = do.call(mxnet::mx.mlp, pars)
    probs = predict(m, data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    p = apply(probs, 2, function(i) {
      w = which.max(i)
      return(ifelse(length(w > 0), w, NaN))
    })
    # avoid error when only one class is predicted
    p = factor(p, exclude = NaN)
    levels(p) = binaryclass.class.levs
    p2 = t(probs)
    colnames(p2) = binaryclass.class.levs
    old.predicts.list[[i]] = p
    # only save probs of positive class
    old.probs.list[[i]] = p2[, binaryclass.task$task.desc$positive]
  }

  set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("classif.mxff", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list.mlr)
  set.seed(getOption("mlr.debug.seed"))
  testProbParsetsWithTol("classif.mxff", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list.mlr)

  # multiclass tests
  old.predicts.list = list()
  old.probs.list = list()

  set.seed(getOption("mlr.debug.seed"))
  for (i in seq_along(parset.list.mxnet)) {
    x = data.matrix(multiclass.train[, -ncol(multiclass.train)])
    y = as.numeric(multiclass.train[, ncol(multiclass.train)]) - 1
    levs = levels(multiclass.df[[multiclass.class.col]])
    pars = c(parset.list.mxnet[[i]], list(data = x, label = y), out_node = length(levs))
    m = do.call(mxnet::mx.mlp, pars)
    probs = predict(m, data.matrix(multiclass.test[, -ncol(multiclass.test)]))
    p = apply(probs, 2, function(i) {
      w = which.max(i)
      return(ifelse(length(w > 0), w, NaN))
    })
    # avoid error when only one class is predicted
    p = factor(p, exclude = NaN)
    levels(p) = levs
    p2 = t(probs)
    colnames(p2) = levs
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("classif.mxff", multiclass.df, multiclass.target, multiclass.train.inds,
    old.predicts.list, parset.list.mlr)
  set.seed(getOption("mlr.debug.seed"))
  testProbParsetsWithTol("classif.mxff", multiclass.df, multiclass.target, multiclass.train.inds,
    old.probs.list, parset.list.mlr)

  # Convoution test
  conv.probs.list = list()
  set.seed(getOption("mlr.debug.seed"))
  x = data.matrix(multiclass.train[, -ncol(multiclass.train)])
  y = as.numeric(multiclass.train[, ncol(multiclass.train)]) - 1
  x = array(aperm(x), dim = c(2, 2, 1, nrow(x)))
  sym = mxnet::mx.symbol.Variable("data")
  sym = mxnet::mx.symbol.Convolution(sym, num.filter = parset.list.conv[[1]]$num.filter,
    kernel = parset.list.conv[[1]]$kernel)
  sym = mxnet::mx.symbol.Pooling(sym, kernel = parset.list.conv[[1]]$kernel,
    pool.type = parset.list.conv[[1]]$pool.type)
  sym = mxnet::mx.symbol.flatten(sym)
  levs = levels(multiclass.df[[multiclass.class.col]])
  sym = mxnet::mx.symbol.FullyConnected(sym, num_hidden = length(levs))
  out = mxnet::mx.symbol.SoftmaxOutput(sym)
  m = mxnet::mx.model.FeedForward.create(out, X = x, y = y, array.layout = "colmajor",
    learning.rate = parset.list.conv[[1]]$learning.rate)
  x.test = data.matrix(multiclass.test[, -ncol(multiclass.test)])
  x.test = array(aperm(x.test), dim = c(2, 2, 1, nrow(x.test)))
  probs = t(predict(m, x.test))
  colnames(probs) = levs
  conv.probs.list[[1]] = probs
  
  set.seed(getOption("mlr.debug.seed"))
  testProbParsetsWithTol("classif.mxff", multiclass.df, multiclass.target, multiclass.train.inds,
    conv.probs.list, parset.list.mlr.conv, tol = 2e-04)
})
