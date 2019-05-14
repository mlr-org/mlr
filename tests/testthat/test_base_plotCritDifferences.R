context("test_CritDifferences")

test_that("test_CritDifferences", {
  lrns = list(makeLearner("classif.rpart"),
    makeLearner("classif.nnet"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("Holdout")
  meas = list(acc, ber)
  res = benchmark(lrns, tasks, rdesc, meas)

  # Case: Make sure rpart is better then nnet in regards to
  #       ber. Minimal p.value for only 2 learners ~.157
  res$results$binary$classif.nnet$aggr[2] = 1
  res$results$multiclass$classif.nnet$aggr[2] = 1

  expect_warning({
    r1 = generateCritDifferencesData(res)
  })
  expect_is(r1, "CritDifferencesData")
  expect_warning({
    r2 = generateCritDifferencesData(res, ber, test = "nemenyi")
  })
  expect_is(r2, "CritDifferencesData")
  r3 = generateCritDifferencesData(res, ber, p.value = 0.5, test = "bd")
  expect_is(r3, "CritDifferencesData")

  # Test Issue #554 (equally performing learners)
  lrns2 = list(makeLearner("classif.rpart", "rpart1"),
    makeLearner("classif.rpart", "rpart2"))
  res2 = benchmark(lrns2, tasks, rdesc, meas)
  expect_warning({
    r4 = generateCritDifferencesData(res2, acc, p.value = 0.3, test = "bd")
  },
  "Learner performances might be exactly equal.")
  expect_is(r4, "CritDifferencesData")

  plotCritDifferences(r1)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r2)
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r3, baseline = "classif.rpart")
  ggsave(tempfile(fileext = ".png"))
  plotCritDifferences(r4)
  ggsave(tempfile(fileext = ".png"))
})
