context("classif_gaterSVM")

test_that("classif_gaterSVM", {
  requirePackagesOrSkip("SwarmSVM", default.method = "load")

  # Early Prediction
  model = SwarmSVM::gaterSVM(x = data.matrix(binaryclass.train[, -61]),
    y = binaryclass.train[, 61], m = 2, max.iter = 1, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[, -61]))
  p = factor(p, labels = levels(binaryclass.train[, 61]))

  testSimple("classif.gaterSVM", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p, parset = list(m = 2, max.iter = 1, seed = 0))

  # Prediction result containing only one class
  data = data.frame(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2), c = c("a", "b", "a", "b"))
  traintask = makeClassifTask("train", data, "c")
  testtask = makeClusterTask("test", data.frame(a = c(1, 1), b = c(1, 1)))
  x = train(makeLearner("classif.gaterSVM", m = 2, seed = 0), traintask)
  result = predict(x, testtask)$data$response
  expect_equal(as.character(result), c("a", "a"))
})
