context("resample_dps")

test_that("dps instance works", {
  desc = makeResampleDesc("DPS", iters = 2)
  rin = makeResampleInstance(desc, sonar.task)

  expect_error(makeResampleInstance(desc, sonar.task, size = 10),
    "One of 'size' or 'task' must be supplied")
  expect_error(makeResampleInstance(desc, size = 10),
    "'size' not supported for DPSDesc objects, please use 'task'")

  folds = rin$desc$iters
  expect_equal(folds, 2)

  size = getTaskSize(sonar.task)
  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= size)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= size)
    expect_equal(sort(c(unique(i1), i2)), seq_len(size))
  }
})

test_that("dps internal functions work", {
  x = getTaskData(sonar.task, target.extra = TRUE)$data
  for (i in 1:floor(log2(nrow(x)))){
    # split the data
    s = doDPSSplits(x, k=i)
    s = do.call("cbind", lapply(s, function(X) X[1:min(sapply(s, length))]))

    # compute distance matrix
    dist1 = fields:::rdist(x)

    # check if the distance of two consecutive sets is increasing
    for (k in seq(1, ncol(s), by=2)) {
      expect_true(!is.unsorted(apply(s, 1, function(X) dist1[X[k], X[k+1]])))
    }
  }
})
