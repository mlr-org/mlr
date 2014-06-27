context("costs")

test_that("costs", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  task = binaryclass.task
  task$task.desc$positive = "M"

  cc = 1 - diag(1, 2)
  rownames(cc) = colnames(cc) = task$task.desc$class.levels
  ms = makeCostMeasure(costs = cc, task = task)
  r = resample(lrn, rdesc, task = task, measures = list(mmce, ms))
  expect_equal(r$aggr[[1]], r$aggr[[2]])

  cc = matrix(0, 2, 2)
  rownames(cc) = colnames(cc) = task$task.desc$class.levels
  cc["R","M"] = 1
  ms = makeCostMeasure(costs = cc, task = task, combine = sum)
  r = resample(lrn, rdesc, task = task, measures = list(fp, ms))
  expect_equal(r$aggr[[1]], r$aggr[[2]])
})
