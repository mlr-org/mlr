context("BMRhypothesisTests")

test_that("hypothesisTest", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  # For friedman.test
  r1 = friedmanTestBMR(res, acc)
  expect_is(r1, "htest")
  r2 = friedmanTestBMR(res, ber, "mean")
  expect_is(r2, "htest")

  # For posthocNemenyiTest
  r = friedmanPostHocTestBMR(res, acc, p.value = 10^(-10))
  expect_is(r, "htest")
  expect_false(r$f.rejnull)
  r = friedmanPostHocTestBMR(res, acc, p.value = .99)
  if (r$p.value < .99) {
    expect_is(r, "pairwise.htest")
    expect_true(r$f.rejnull)
  }
  expect_is(r$crit.difference[[1]], "numeric")
  expect_more_than(r$crit.difference[[1]], 0L)
})
