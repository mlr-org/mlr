context("BenchmarkSummary")

test_that("BenchmarkSummary", {
  
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(iris.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, timeboth)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))  
  
  # For generateData
  r1 = generateBenchmarkSummaryData(res, acc, fill = "best")
  expect_is(r1, "BenchmarkSummaryData")
  r2 = generateBenchmarkSummaryData(res, ber, fill = "worst")
  expect_is(r2, "BenchmarkSummaryData")  
  r3 = generateBenchmarkSummaryData(res, timeboth, fill = "worst")
  
  
  # For plot
  p = plotBenchmarkSummary(r1)
  expect_is(p, "ggplot")
  p = plotBenchmarkSummary(r2)
  expect_is(p, "ggplot")
  p = plotBenchmarkSummary(r3)
  expect_is(p, "ggplot")
  
  
})
