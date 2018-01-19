context("costs")

test_that("costs", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  task = binaryclass.task
  task$task.desc$positive = "M"

  cc = 1 - diag(1, 2)
  rownames(cc) = colnames(cc) = getTaskClassLevels(task)
  ms = makeCostMeasure(costs = cc)
  r = resample(lrn, rdesc, task = task, measures = list(mmce, ms))
  expect_equal(r$aggr[[1]], r$aggr[[2]])

  cc = matrix(0, 2, 2)
  rownames(cc) = colnames(cc) = getTaskClassLevels(task)
  cc["R", "M"] = 1
  ms = makeCostMeasure(id = "foo", costs = cc, combine = sum)
  expect_equal(ms$id, "foo")
  r = resample(lrn, rdesc, task = task, measures = list(fp, ms))
  expect_equal(r$aggr[[1]], r$aggr[[2]])
})
