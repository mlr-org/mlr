context("BenchmarkResult")

test_that("BenchmarkResult", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  plotBenchmarkResult(res)
})
