context("BenchmarkResult")

test_that("BenchmarkResult", {
  
  # Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  
  # For plotRankMatrixAsBar
  p = plotBenchmarkResult(res)
  expect_is(p, "ggplot")
  p = plotBenchmarkResult(res,ber, order.Tsks = c(2L,3L,1L))
  expect_is(p, "ggplot")
  p = plotBenchmarkResult(res,mmce, order.Lrns = c(1L,4L,3L,2L))
  expect_is(p, "ggplot")
  
})
