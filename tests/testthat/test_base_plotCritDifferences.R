context("plotcritDifferences")

test_that("critDifferences", {
  
  lrns = list(makeLearner("classif.rpart"),
              makeLearner("classif.nnet"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  r1 = generateCritDifferencesData(res)
  expect_is(r1, "critDifferencesData")
  r2 = generateCritDifferencesData(res, ber, baseline = "classif.rpart", test = "nemenyi")
  expect_is(r2, "critDifferencesData")
  r3 = generateCritDifferencesData(res, ber, p.value = 0.1, test = "bd")
  expect_is(r3, "critDifferencesData")
  r3$cd.info$textvjust = 1L
  expect_message(generateCritDifferencesData(res, mmce, p.value = 10^(-10)))
  # Test Issue #554
  set.seed(123L)
  res = benchmark(lrns, tasks, rdesc, meas)
  r = generateCritDifferencesData(res, mmce, p.value = 0.3, test = "bd")
  expect_is(r, "critDifferencesData")
  
  plotCritDifferences(r1)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r2)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r3,  baseline = "classif.rpart")
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r)
  ggsave(tempfile(fileext = ".png"))
})
