context("classif_mlp")

test_that("classif_mlp", {
  requirePackagesOrSkip("RSNNS", default.method = "load")

  # test with empty paramset
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = RSNNS::decodeClassLabels(binaryclass.train[, ncol(binaryclass.train)])
    m = RSNNS::mlp(x = x, y = y)
    p = predict(m, data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    p = max.col(p)
    p = factor(p, labels = binaryclass.class.levs)
  })

  testSimple("classif.mlp", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p, parset = list())

  # test with params passed
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
    y = RSNNS::decodeClassLabels(binaryclass.train[, ncol(binaryclass.train)])
    m = RSNNS::mlp(x = x, y = y, size = 7, maxit = 100)
    p = predict(m, data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
    p = max.col(p)
    p = factor(p, labels = binaryclass.class.levs)
  })

  testSimple("classif.mlp", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p,
    parset = list(size = 7, maxit = 100))
})
