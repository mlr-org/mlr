context("downsample")

test_that("downsample",  {
  down.tsk = downsample(multiclass.task, perc = 1/3)
  expect_equal(down.tsk$task.desc$size, 50L)
  rsm.methods = c("Bootstrap", "Subsample", "Holdout")
  for(rsm.method in rsm.methods) {
    rin = makeResampleInstance(rsm.method, task = binaryclass.task)
    rin2 = downsample(rin, perc = 0.5)
    sapply(seq_along(rin$train.inds), function(i)
      expect_equal(
        length(rin2$train.inds[[i]]),
        length(rin$train.inds[[i]]) / 2
      )
    )
  }
})

test_that("downsample wrapper",  {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn = makeDownsampleWrapper("classif.rpart", dw.perc = 0.5)
  r = resample(lrn, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})


