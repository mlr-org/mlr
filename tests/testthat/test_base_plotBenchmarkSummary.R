context("BenchmarkSummary")

test_that("BenchmarkSummary", {
  
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(iris.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))  
  
  # For generateData
  r1 = generateBenchmarkSummaryData(res, acc, fill = "best")
  expect_is(r1, "BenchmarkSummaryData")
  r2 = generateBenchmarkSummaryData(res, ber, fill = "worst")
  expect_is(r2, "BenchmarkSummaryData")  
  
  
  # For plot
  p = plotBenchmarkSummary(r11)
  expect_is(p, "ggplot")
  p = plotBenchmarkSummary(r12)
  expect_is(p, "ggplot")
  
  
})
