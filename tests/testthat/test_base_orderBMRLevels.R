context("orderBMRLevels")

test_that("RankMatrix", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)

  # Create vector with learner.ids and task.ids
  lrn.id1 = c("classif.nnet", "classif.rpart")
  lrn.id2 = getBMRLearnerIds(res)[c(1L, 2L)]
  tsk.id1 = getBMRTaskIds(res)[c(1L, 2L)]

  # Test ordering Lrns
  # Test Class
  r1 = orderBMRLrns(res, order.lrns = lrn.id1)
  expect_is(r1, "data.frame")
  r2 = orderBMRLrns(res, order.lrns = lrn.id2)
  expect_is(r2, "data.frame")
  # Test constant dimensions
  expect_equal(dim(r1), dim(as.data.frame(res)))
  expect_equal(dim(r2), dim(as.data.frame(res)))

  # Test ordering Tsks
  r3 = orderBMRTasks(res, order.tsks = tsk.id1)
  expect_is(r3, "data.frame")
  expect_equal(dim(r2), dim(as.data.frame(res)))
})
