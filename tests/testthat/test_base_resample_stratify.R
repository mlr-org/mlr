context("resample_stratify")

test_that("stratification instances work", {
  mytest = function(rin, size1, size2) {
    for (i in 1:rin$desc$iters) {
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      if (!is.null(size1)) {
        expect_true(all(as.numeric(table(getTaskTargets(multiclass.task)[i1])) == size1))
      }
      if (!is.null(size2)) {
        expect_true(all(as.numeric(table(getTaskTargets(multiclass.task)[i2])) == size2))
      }
      expect_equal(sort(c(unique(i1), i2)), 1:150)
    }
  }

  expect_error(makeResampleDesc("LOO", stratify = TRUE), "Stratification cannot")

  rin = makeResampleInstance(makeResampleDesc("Holdout", stratify = TRUE),
    task = multiclass.task)
  mytest(rin, 33, 17)

  rin = makeResampleInstance(makeResampleDesc("Subsample", iters = 3, split = 0.5,
    stratify = TRUE),
  task = multiclass.task)
  mytest(rin, 25, 25)

  rin = makeResampleInstance(makeResampleDesc("CV", iters = 10, stratify = TRUE),
    task = multiclass.task)
  mytest(rin, 45, 5)

  rin = makeResampleInstance(makeResampleDesc("RepCV", reps = 2, folds = 5,
    stratify = TRUE), task = multiclass.task)
  mytest(rin, 40, 10)

  rin = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 1,
    stratify = TRUE), task = multiclass.task)
  mytest(rin, 50, NULL)
})

test_that("stratification with empty classes works", {
  task = subsetTask(multiclass.task, subset = 1:100)
  rdesc = makeResampleDesc("Holdout", split = 0.3, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = task)
  expect_equal(length(rin$train.inds[[1]]), 30)
  expect_equal(length(rin$test.inds[[1]]), 70)
  expect_true(all(rin$train.inds[[1]] %in% 1:100))
  expect_true(all(rin$test.inds[[1]] %in% 1:100))
})

test_that("stratification does not put all remaining elements in the first split", {
  k = 50
  sizes = rep(7, k)
  n = 7 * k

  data = data.frame(x = runif(n), y = factor(rep(1:k, sizes)))
  task = makeClassifTask(data = data, target = "y")
  rdesc = makeResampleDesc("CV", iters = 3, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = task)

  tabs = lapply(rin$test.inds, function(j) table(getTaskTargets(task)[j]))
  split.sizes = sapply(tabs, sum)
  expect_true(all(split.sizes < 130))
})

test_that("stratification with survival works", {
  df = data.frame(time = 1:4, event = rep(0:1, 2), x = rnorm(4))
  task = makeSurvTask(data = df, target = c("time", "event"))
  rdesc = makeResampleDesc("Holdout", split = 0.5, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = task)
  expect_true(setequal(df$event[rin$train.inds[[1]]], 0:1))
  expect_true(setequal(df$event[rin$test.inds[[1]]], 0:1))
})

test_that("stratification on features work", {
  df = data.frame(x = rep(c("a", "b"), each = 4), y = rep(c("a", "b"), times = 4), z = 1:8)
  task = makeRegrTask(data = df, target = "z")
  rdesc = makeResampleDesc("Holdout", split = 0.5, stratify.cols = c("x", "y"))
  rin = makeResampleInstance(rdesc, task = task)
  train = df[rin$train.inds[[1]], ]
  test = df[rin$test.inds[[1]], ]
  expect_true(setequal(apply(train[c("x", "y")], 1, collapse, sep = ""),
    c("aa", "ab", "ba", "bb")))
  expect_true(setequal(apply(test[c("x", "y")], 1, collapse, sep = ""),
    c("aa", "ab", "ba", "bb")))
})

test_that("stratification on integers work", {
  df = data.frame(x = rep(c("a", "b"), each = 4), y = rep(c("a", "b"), times = 4),
    z = rep(1:2, each = 4))
  task = makeClassifTask(data = df, target = "y")
  rdesc = makeResampleDesc("Holdout", split = 0.5, stratify.cols = "z")
  rin = makeResampleInstance(rdesc, task = task)
  train = df[rin$train.inds[[1]], ]
  test = df[rin$test.inds[[1]], ]
  expect_equal(as.integer(table(train$z)), c(2L, 2L))
  expect_equal(as.integer(table(test$z)), c(2L, 2L))
})

test_that("stratification on doubles does not work", {
  df = data.frame(x = rep(c("a", "b"), each = 4), y = rep(c("a", "b"),
    times = 4), z = rep(1:2, each = 4))
  df$z = as.double(df$z)
  task = makeClassifTask(data = df, target = "y")
  rdesc = makeResampleDesc("Holdout", split = 0.5, stratify.cols = "z")
  expect_error(makeResampleInstance(rdesc, task = task), "double-precision")
})
