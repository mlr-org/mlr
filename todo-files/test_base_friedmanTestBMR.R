context("test_friedmanTestBMR")

test_that("test_friedmanTestBMR", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("Holdout")
  meas = list(acc, ber)
  res = benchmark(lrns, tasks, rdesc, meas)

  # For friedmanTest
  r1 = friedmanTestBMR(res, acc)
  expect_is(r1, "htest")
  r2 = friedmanTestBMR(res, ber, "mean")
  expect_is(r2, "htest")

  # For friedmanPostHocTest
  # Case: Do not reject null
  expect_warning({r3 = friedmanPostHocTestBMR(res, acc, p.value = 10^ (-10))})
  expect_is(r3, "htest")
  expect_false(r3$f.rejnull)

  # Case: Reject null
  # Make sure nnet is always worse then rpart. (add error)
  res$results$binary$classif.nnet$measures.test$acc = 1
  res$results$multiclass$classif.nnet$measures.test$acc = 1
  expect_warning({r4 = friedmanPostHocTestBMR(res, acc, p.value = .99)})
  if (r4$p.value < .99) {
    expect_is(r4, "PMCMR")
    expect_true(r4$f.rejnull)
  }
  expect_is(r4$crit.difference[[1]], "numeric")
  expect_gt(r4$crit.difference[[1]], 0L)
})


test_that("Friedman test on one learner / one task", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("Holdout")

  res = benchmark(lrns, tasks[[1]], rdesc)
  expect_error(friedmanTestBMR(res), "at least two tasks")
  expect_error(friedmanPostHocTestBMR(res), "at least two tasks")
  expect_error(generateCritDifferencesData(res), "at least two tasks")

  res = benchmark(lrns[[1]], tasks, rdesc)
  expect_error(friedmanTestBMR(res), "at least two learners")
  expect_error(friedmanPostHocTestBMR(res), "at least two learners")
  expect_error(generateCritDifferencesData(res), "at least two learners")
})
