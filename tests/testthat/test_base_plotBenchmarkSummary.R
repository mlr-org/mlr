context("BenchmarkSummary")

test_that("BenchmarkSummary", {
  
  # Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 5L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))  
  
  # For generateData
  r_1 = generateBenchmarkSummaryData(res,acc,fill = "best")
  expect_is(r, "BenchmarkSummaryData")
  r_2 = generateBenchmarkSummaryData(res,ber,fill = "worst")
  expect_is(r, "BenchmarkSummaryData")  
  
  
  # For plot
  p = plotBenchmarkSummary(r_1)
  expect_is(p, "ggplot")
  p = plotBenchmarkSummary(r_2)
  expect_is(p, "ggplot")
  
  
})
