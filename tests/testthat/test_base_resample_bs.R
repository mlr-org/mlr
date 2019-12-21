context("resample_bs")

test_that("bs instance works", {
  rin = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 25)

  iters = rin$desc$iters
  expect_equal(iters, 3)

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_equal(length(i1), 25)
    expect_equal(length(i2), 25 - length(unique(i1)))
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
})

test_that("bs resampling works", {
  data = multiclass.df
  formula = multiclass.formula
  parset = list(minsplit = 12, cp = 0.09)

  requirePackagesOrSkip("rpart", default.method = "load")
  tt = function(formula, data, subset) {
    rpart::rpart(formula, data = data[subset, ], minsplit = 12, cp = 0.09)
  }
  tp = function(model, newdata) {
    predict(model, newdata, type = "class")
  }
  testBootstrap("classif.rpart", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset = parset)
})

test_that("bs instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 25)

  iters = rin$desc$iters
  expect_equal(iters, 3)

  for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_equal(length(i1), 25)
    expect_equal(length(i2), 25 - length(unique(i1)))
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
  rin1 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 500)
  rin2 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 3), size = 500)
  expect_true(!all(sort(rin1$train.inds[[1]]) == sort(rin2$train.inds[[1]])))
})
