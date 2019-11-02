context("convertBMRToRankMatrix")

test_that("convertBMRToRankMatrix", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  n.tsks = length(getBMRTaskIds(res))
  n.lrns = length(getBMRLearnerIds(res))

  # measure = NULL
  r = convertBMRToRankMatrix(res)
  expect_is(r, "matrix")
  expect_equal(dim(r), c(n.lrns, n.tsks))
  expect_equivalent(colnames(r), getBMRTaskIds(res))
  expect_equivalent(rownames(r), getBMRLearnerIds(res))
  expect_equal(sum(r), sum(1:n.lrns * n.tsks))

  # measure = ber
  r = convertBMRToRankMatrix(res, ber)
  expect_is(r, "matrix")
  expect_equal(dim(r), c(n.lrns, n.tsks))
  expect_equivalent(rownames(r), getBMRLearnerIds(res))
  expect_equal(sum(r), sum(1:n.lrns * n.tsks))

  # check ties.method
  r = convertBMRToRankMatrix(res, featperc, ties.method = "first")
  expect_equal(as.numeric(r[, 1]), 1:2)
  expect_equal(as.numeric(r[, 2]), 1:2)
  r = convertBMRToRankMatrix(res, featperc, ties.method = "average")
  expect_equal(as.numeric(r[, 1]), c(1.5, 1.5))
  expect_equal(as.numeric(r[, 2]), c(1.5, 1.5))

  # check that col and row names are right if only one task is given
  res = benchmark(lrns, binaryclass.task, rdesc, meas)
  r = convertBMRToRankMatrix(res)
  expect_equivalent(rownames(r), getBMRLearnerIds(res))
  expect_equivalent(colnames(r), getBMRTaskIds(res))
})
