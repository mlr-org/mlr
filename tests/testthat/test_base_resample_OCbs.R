context("resample_ocbs")

test_that("ocbs instance works", {
  rin = makeResampleInstance(makeResampleDesc("OCBootstrap", iters = 3), oneclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 3)

  label = getTaskTargets(oneclass.task)
  size = getTaskSize(oneclass.task)
  normal.inds = which(label == oneclass.task$task.desc$negative)
  normal.size = length(normal.inds)
  anomaly.inds = which(label == oneclass.task$task.desc$positive)
  anomaly.size = length(anomaly.inds)
  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    # sample with replacement with the size of the normal data
    expect_equal(length(i1), size)
    #in test are the rest of normal obs and all anomaly data
    expect_equal(length(i2), (normal.size - length(unique(i1)) +  anomaly.size))
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 100)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 105)
    expect_equal(sort(c(unique(i1), i2)), 1:105)
  }
})


test_that("ocbs instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("OCBootstrap", iters = 3), oneclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 3)

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
  }
  rin1 = makeResampleInstance(makeResampleDesc("OCBootstrap", iters = 3), oneclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("OCBootstrap", iters = 3), oneclass.task)
  expect_true(!all(sort(rin1$train.inds[[1]]) == sort(rin2$train.inds[[1]])))
})
