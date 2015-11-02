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
  # Test Issue #554
  r3 = generateCritDifferencesData(res, ber, p.value = 0.1, test = "bd")
  r3$cd.info$textvjust = 1L
  expect_is(r3, "critDifferencesData")
  r3$baseline = res$learner.id
  expect_message(generateCritDifferencesData(res, mmce, p.value = 10^(-10)))
  plotCritDifferences(r)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r1)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r2)
  ggsave(tempfile(fileext = ".png"))
})
