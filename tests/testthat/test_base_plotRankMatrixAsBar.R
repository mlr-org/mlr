context("RankMatrix")

test_that("RankMatrix", {

  # Get Data
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(iris.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  n.tsks = length(getBMRTaskIds(res))
  n.lrns = length(getBMRLearnerIds(res))
  
  # test for convertBMRToRankMatrix
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
  
  # aggregation = "mean"
  r = convertBMRToRankMatrix(res, aggregation = "mean")
  expect_is(r, "matrix")
  expect_equal(dim(r), c(n.lrns, n.tsks))
  expect_equivalent(rownames(r), getBMRLearnerIds(res))
  expect_equal(sum(r), sum(1:n.lrns * n.tsks))

  # For generateRankMatrixAsBarData
  r = generateRankMatrixAsBarData(res)
  expect_is(r, "RankMatrixAsBarData")
  r2 = generateRankMatrixAsBarData(res, acc)
  expect_is(r2, "RankMatrixAsBarData")
  r3 = generateRankMatrixAsBarData(res, featperc)
  expect_is(r3, "RankMatrixAsBarData") 
  
  # For plotRankMatrixAsBar
  p = plotRankMatrixAsBar(r)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r2)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r3)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r2, pos = "stack")
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r, pos = "dodge")
  expect_is(p, "ggplot")

})

