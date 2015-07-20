context("BenchmarkResult")

test_that("BenchmarkResult", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  plotBenchmarkResult(res)
})

test_that("BenchmarkResult allows spaces", {
  cv10f = makeResampleDesc("CV", iters = 2L)
  measures = list(mlr::auc)
  learners = list(
    makeLearner("classif.rpart", predict.type = "prob")
  )

  res = benchmark(learners, sonar.task, cv10f, measures)

  plotBenchmarkResult(res, measure=auc)
})
