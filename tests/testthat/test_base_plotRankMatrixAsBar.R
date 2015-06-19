context("RankMatrix")

test_that("RankMatrix", {

  # Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))
  
  # Test for convertBMRToRankMatrix
  # 1) measure = NULL
  r = convertBMRToRankMatrix(res)
  expect_is(r, "data.frame")
  expect_equal(dim(r), c(nLrns, nTasks + 1))
  expect_equivalent(colnames(r),c("learner.id", getBMRTaskIds(res)))
  expect_equivalent(as.character(r$learner.id), getBMRLearnerIds(res))
  expect_equal(sum(r[, -c(1)]), sum(1:nLrns * nTasks))
  # 2) measure = ber
  r = convertBMRToRankMatrix(res, ber)
  expect_is(r, "data.frame")
  expect_equal(dim(r), c(nLrns, nTasks + 1))
  expect_equivalent(colnames(r), c("learner.id", getBMRTaskIds(res)))
  expect_equivalent(as.character(r$learner.id), getBMRLearnerIds(res))
  expect_equal(sum(r[, -c(1)]), sum(1:nLrns * nTasks))
  # 3) aggregation = "mean"
  r = convertBMRToRankMatrix(res, aggregation = "mean")
  expect_is(r, "data.frame")
  expect_equal(dim(r), c(nLrns, nTasks + 1))
  expect_equivalent(colnames(r), c("learner.id", getBMRTaskIds(res)))
  expect_equivalent(as.character(r$learner.id), getBMRLearnerIds(res))
  expect_equal(sum(r[, -c(1)]), sum(1:nLrns * nTasks))

  # For generateRankMatrixAsBarData
  r = generateRankMatrixAsBarData(res)
  expect_is(r, "RankMatrixAsBarData")
  r = generateRankMatrixAsBarData(res, acc)
  expect_is(r, "RankMatrixAsBarData")
  r = generateRankMatrixAsBarData(res, featperc)
  expect_is(r, "RankMatrixAsBarData") 
  
  # For plotRankMatrixAsBar
  p = plotRankMatrixAsBar(r)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r)
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r, pos = "tile")
  expect_is(p, "ggplot")
  p = plotRankMatrixAsBar(r, pos = "dodge")
  expect_is(p, "ggplot")

})

