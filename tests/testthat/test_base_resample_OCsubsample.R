context("resample_OCsubsample")

test_that("OCsubsampling instance works", {
  rin = makeResampleInstance(makeResampleDesc("OCSubsample", iters = 2, split = 0.25), oneclass.task)
  iters = rin$desc$iters
  expect_equal(iters, 2)

  label = getTaskTargets(oneclass.task)
  size = getTaskSize(oneclass.task)
  anomaly.inds = which(label == oneclass.task$task.desc$positive)
  normal.inds = which(label == oneclass.task$task.desc$negative)
  normal.size = length(normal.inds)
  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_equal(length(i1), floor(0.25 * size))
    expect_equal(length(i2), size - floor(0.25 * size))

    expect_true(min(i1) >= 1)
    #sample without replacement, train data can't have more observations than the number of normal observation
    expect_true(max(i1) <= normal.size)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= size)

    # does training only have normal data?
    expect_false(any(i1 %nin% normal.inds))

    # does testing contain all anomalies
    expect_false(any(anomaly.inds %nin% i2))
  }
})


test_that("subsampling instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("OCSubsample", iters = 2, split = 0.25), oneclass.task)
  iters = rin$desc$iters

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
  }
  rin1 = makeResampleInstance(makeResampleDesc("OCSubsample", iters = 3), oneclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("OCSubsample", iters = 3), oneclass.task)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})
