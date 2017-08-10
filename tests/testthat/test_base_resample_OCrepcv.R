context("resample_ocrepcv")

test_that("test.join works somehow", {
  df = data.frame(t = factor(rep(c("a", "b"), each = 4)), x = 9:6)
  task = makeOneClassTask(data = df, target = "t", positive = "a", negative = "b")
  lrn = makeLearner("oneclass.svm")
  measures = list(fn, setAggregation(fn, test.join))
  rin = makeResampleInstance(makeResampleDesc("OCRepCV", reps = 5, folds = 3), task = task)
  res = resample(learner = lrn, task = task, resampling = rin,
    measures = measures, models = FALSE, weights = NULL, keep.pred = TRUE)
  expect_equal(res$measures.test[, 2L], res$measures.test[, 3L])
  expect_true(diff(res$aggr) > 0)

  lrn = setPredictType(lrn, predict.type = "prob")
  res.prob = resample(learner = lrn, task = task, resampling = rin, measures = measures)
  expect_equal(res.prob$measures.test[, 2L], res.prob$measures.test[, 3L])
})

test_that("repcv instance works", {
  rin = makeResampleInstance(makeResampleDesc("OCRepCV", folds = 10, reps = 3), task = oneclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 10 * 3)
  reps = rin$desc$reps
  expect_equal(reps, 3)

  normal.inds = which(oneclass.task$env$data$Target == oneclass.task$task.desc$negative)

  for (j in 1:3) {
    bag = NULL
    for (i in 1:10) {
      k = as.integer((j - 1) * 10L + i)
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      #expect_equal(length(unique(i2)), 15)
      bag = c(bag, i2)
      # does training only have normal data?
      expect_false(any(i1 %nin% normal.inds))
    }
    expect_equal(sort(unique(bag)), 1:105)
  }
})

test_that("ocrepcv resampling works", {
  rdesc = makeResampleDesc("OCRepCV", folds = 2, reps = 2)
  m = setAggregation(mmce, testgroup.mean)
  resample(makeLearner("oneclass.svm"), oneclass.task, rdesc)
})

test_that("repcv instance is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("OCRepCV", folds = 10, reps = 3), task = oneclass.task)

  iters = rin$desc$iters
  expect_equal(iters, 10 * 3)
  reps = rin$desc$reps
  expect_equal(reps, 3)

  for (j in 1:3) {
    bag = NULL
    for (i in 1:10) {
      k = as.integer((j - 1) * 10L + i)
      i1 = rin$train.inds[[i]]
      i2 = rin$test.inds[[i]]
      bag = c(bag, i2)
    }
    expect_equal(sort(unique(bag)), 1:105)
  }
  rin1 = makeResampleInstance(makeResampleDesc("OCRepCV", folds = 2, reps = 2), oneclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("OCRepCV", folds = 2, reps = 2), oneclass.task)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})

