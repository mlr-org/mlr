context("critDifferences")

test_that("critDifferences", {
  
  # Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  # For generateCritDifferencesData
  r_1 = generateCritDifferencesData(res)
  expect_is(r, "critDifferencesData")
  r_2 = generateCritDifferencesData(res,ber,baseline = "classif.nnet")
  expect_is(r, "critDifferencesData")
  r = generateCritDifferencesData(res,mmce, p.value = 0.3)
  expect_is(r, "critDifferencesData")
  expect_message(generateCritDifferencesData(res,mmce, p.value = 10^(-10)))
  
  
  # For plotCritDifferences
  p = plotCritDifferences(r)
  expect_is(p, "ggplot")
  p = plotCritDifferences(r_1)
  expect_is(p, "ggplot")
  p = plotCritDifferences(r_2)
  expect_is(p, "ggplot")
  

  })

