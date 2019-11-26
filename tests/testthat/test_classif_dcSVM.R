context("classif_dcSVM")

test_that("classif_dcSVM", {
  requirePackagesOrSkip("SwarmSVM", default.method = "load")

  # Early Prediction
  model = SwarmSVM::dcSVM(x = data.matrix(binaryclass.train[, -61]), y = binaryclass.train[, 61],
    m = 100, k = 10, max.levels = 1, early = 1, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[, -61]))
  p = factor(p, labels = levels(binaryclass.train[, 61]))

  testSimple("classif.dcSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
    parset = list(m = 100, k = 10, max.levels = 1, early = 1, seed = 0))

  # Exact Prediction
  # suppressed Warning: "max.levels reduced"
  model = suppressWarnings(SwarmSVM::dcSVM(x = data.matrix(binaryclass.train[, -61]),
    y = binaryclass.train[, 61], m = 100, k = 2, max.levels = 3, early = 0, seed = 0))
  p = predict(model, data.matrix(binaryclass.test[, -61]))
  p = factor(p, labels = levels(binaryclass.train[, 61]))

  # suppressed Warning: "max.levels reduced"
  suppressWarnings(
    testSimple("classif.dcSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
      parset = list(m = 100, k = 2, max.levels = 3, early = 0, seed = 0))
  )

  # Prediction result containing only one class
  data = data.frame(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2), c = c("a", "b", "a", "b"))
  traintask = makeClassifTask("train", data, "c")
  testtask = makeClusterTask("test", data.frame(a = c(1, 1), b = c(1, 1)))
  x = train(makeLearner("classif.dcSVM", seed = 0), traintask)
  result = predict(x, testtask)$data$response
  expect_equal(as.character(result), c("a", "a"))
})

test_that("classif_dcSVM works correctly when e1071 is used (#733)", {
  requirePackagesOrSkip("SwarmSVM", default.method = "load")

  expect_silent(train(makeLearner("classif.dcSVM", kernel = 1), binaryclass.task))
})
