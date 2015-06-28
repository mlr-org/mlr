context("BenchmarkResult")

test_that("BenchmarkResult", {
  
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(iris.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  
  # For plotRankMatrixAsBar
  p = plotBenchmarkResult(res)
  expect_is(p, "ggplot")
  p = plotBenchmarkResult(res, ber)
  expect_is(p, "ggplot")
  p = plotBenchmarkResult(res, mmce)
  expect_is(p, "ggplot")
  
})
