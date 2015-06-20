context("hypothesisTest")

test_that("hypothesisTest", {
  # Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  
  # For friedman.test
  r_1 = friedmanTestBMR(res,acc)
  expect_is(r_1,"htest")
  r_2 = friedmanTestBMR(res,ber,"mean")
  expect_is(r_2,"htest") 
  
  
  # For posthocNemenyiTest
  r = posthocNemenyiTestBMR(res,acc, p.value = 10^(-10))
  expect_is(r,"htest") 
  expect_false(r$fRejNull)
  r = posthocNemenyiTestBMR(res,acc, p.value = 0.99)
  expect_is(r,"pairwise.htest") 
  expect_true(r$fRejNull)
  expect_is(r$cDifference,"numeric")
  expect_more_than(r$cDifference, 0L)
  
  })
