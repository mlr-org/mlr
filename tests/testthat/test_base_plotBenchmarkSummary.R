context("BenchmarkSummary")

test_that("BenchmarkSummary", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, timeboth)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))

  r1 = generateBenchmarkSummaryData(res, acc, fill = "best")
  expect_is(r1, "BenchmarkSummaryData")
  r2 = generateBenchmarkSummaryData(res, ber, fill = "worst")
  expect_is(r2, "BenchmarkSummaryData")
  r3 = generateBenchmarkSummaryData(res, timeboth, fill = "worst")

  plotBenchmarkSummary(r1)
  plotBenchmarkSummary(r2)
  plotBenchmarkSummary(r3)
})
