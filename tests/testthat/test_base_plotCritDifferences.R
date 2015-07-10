context("critDifferences")

test_that("critDifferences", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  r = generateCritDifferencesData(res, mmce, p.value = 0.3, test = "bd")
  expect_is(r, "critDifferencesData")
  r1 = generateCritDifferencesData(res)
  expect_is(r1, "critDifferencesData")
  r2 = generateCritDifferencesData(res, ber, baseline = "classif.nnet", test = "nemenyi")
  expect_is(r2, "critDifferencesData")
  expect_message(generateCritDifferencesData(res, mmce, p.value = 10^(-10)))

  plotCritDifferences(r)
  plotCritDifferences(r1)
  plotCritDifferences(r2)
})
