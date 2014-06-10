context("downsample")

test_that("downsample",  {
  down.tsk = downsample(multiclass.task, select = "abs", val = 50L)
  expect_equal(down.tsk$task.desc$size, 50L)
  rsm.methods = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")
  for(rsm.method in rsm.methods) {
    res = makeResampleDesc(method = rsm.method)
    res.inst = makeResampleInstance(desc = res, task = binaryclass.task)
    res.inst.down = downsample(res.inst, select = "perc", val = 0.5)
  }
})

test_that("downsample wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeDownsampleWrapper(lrn1, dw.select = "perc", dw.val = 0.5)
  lrn3 = makeDownsampleWrapper(lrn1, dw.select = "abs", dw.val = 50, dw.stratify = TRUE)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  r = resample(lrn3, multiclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})


