
test_that("PreprocWrapperICA", {
  lrn1 = makeLearner("classif.rpart", minsplit = 10)
  lrn2 = makePreprocWrapperICA(lrn1, lrate = 0.3)
  m = train(lrn2, multiclass.task)
  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)
})


test_that("PreprocWrapperICA works with factors and params", {
  f = function() as.factor(sample(1:2, 100, replace = TRUE))
  data = data.frame(x1 = f(), x2 = runif(100), x3 = runif(100), y = f())
  task = makeClassifTask(data = data, target = "y")
  lrn1 = makeLearner("classif.multinom")
  lrn2 = makePreprocWrapperICA(lrn1, epochs = 10L, lrate = 1, ncomp = 1L) # only one component!
  m = train(lrn2, task)
  p = predict(m, task)
  perf = performance(p, mmce)
  expect_equal(getLeafModel(m)$features, c("x1", "V1"))
  expect_true(!is.na(perf))

  data = data.frame(x1 = f(), x2 = runif(100), y = f())
  task = makeClassifTask(data = data, target = "y")
  lrn1 = makeLearner("classif.multinom")
  lrn2 = makePreprocWrapperICA(lrn1, epochs = 10L, lrate = 1.5, fun = "positive kurtosis")
  m = train(lrn2, task)
  p = predict(m, task)
  perf = performance(p, mmce)
  expect_equal(getLeafModel(m)$features, c("x1", "V1"))
  expect_true(!is.na(perf))

  data = data.frame(x1 = f(), x2 = f(), y = f())
  task = makeClassifTask(data = data, target = "y")
  lrn1 = makeLearner("classif.multinom")
  lrn2 = makePreprocWrapperICA(lrn1, epochs = 10L, lrate = 0.5)
  m = train(lrn2, task)
  p = predict(m, task)
  perf = performance(p, mmce)
  expect_equal(getLeafModel(m)$features, c("x1", "x2"))
  expect_true(!is.na(perf))
})
