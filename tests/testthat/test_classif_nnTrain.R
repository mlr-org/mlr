context("classif_nnTrain")

test_that("classif_nnTrain", {
  requirePackagesOrSkip("deepnet", default.method = "load")

  # test with empty paramset
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = binaryclass.train[, ncol(binaryclass.train)]

    dict = sort(unique(y))
    onehot = matrix(0, length(y), length(dict))
    for (i in seq_along(dict)) {
      ind = which(y == dict[i])
      onehot[ind, i] = 1
    }

    m = deepnet::nn.train(x = x, y = onehot, output = "softmax")
    p = deepnet::nn.predict(m,
      data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    colnames(p) = binaryclass.class.levs
    p = as.factor(colnames(p)[max.col(p)])
  })

  testSimple("classif.nnTrain", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p,
    parset = list())

  # test with params passed
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = binaryclass.train[, ncol(binaryclass.train)]

    dict = sort(unique(y))
    onehot = matrix(0, length(y), length(dict))
    for (i in seq_along(dict)) {
      ind = which(y == dict[i])
      onehot[ind, i] = 1
    }

    m = deepnet::nn.train(x = x, y = onehot, hidden = 7, output = "softmax")
    p = deepnet::nn.predict(m,
      data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    colnames(p) = binaryclass.class.levs
    p = as.factor(colnames(p)[max.col(p)])
  })

  testSimple("classif.nnTrain", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p,
    parset = list(hidden = 7))

  lrn = makeLearner("classif.nnTrain", max.number.of.layers = 2, hidden = 1:3)
  m = getLearnerModel(train(lrn, binaryclass.task))
  expect_equal(m$hidden, 1:2)
})
