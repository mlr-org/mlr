context("downsample")

test_that("downsample",  {
  down.df = downsample(binaryclass.df, target = binaryclass.target, percentage=0.5, stratify=TRUE)
  expect_equal(binaryclass.df[as.numeric(rownames(down.df)), ], down.df) #quite strict
  down.tsk = downsample(multiclass.task, n = 50)
  expect_equal(down.tsk$task.desc$size, 50L)
  rsm.methods = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")
  for(rsm.method in rsm.methods) {
    res = makeResampleDesc(method = rsm.method)
    res.inst = makeResampleInstance(desc = res, task = binaryclass.task)
    res.inst.down = downsample(res.inst, percentage=0.5)
  }
})

test_that("downsample wrapper",  {
  rdesc = makeResampleDesc("CV", iters=2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeDownsampleWrapper(lrn1, sw.percentage = 0.5)
  lrn3 = makeDownsampleWrapper(lrn1, sw.n = 50, sw.stratify = TRUE)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
  r = resample(lrn3, multiclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})


