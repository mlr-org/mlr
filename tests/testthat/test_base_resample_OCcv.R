context("resample_occv")

test_that("occv require task", {
  expect_error(makeResampleInstance(makeResampleDesc("OCCV", iters = 3)), "For resampling for oneclass-classification 'task' must be supplied")
  rdesc = makeResampleDesc("OCCV", iters = 3)
  expect_error(resample("oneclass.svm",  resamplin = rdesc, show.info = TRUE), "argument \"task\" is missing, with no default")
})

test_that("occv instance works", {
  rin = makeResampleInstance(makeResampleDesc("OCCV", iters = 3), oneclass.task)

  folds = rin$desc$iters
  expect_equal(folds, 3)
  normal.inds = which(oneclass.task$env$data$Target == oneclass.task$task.desc$negative)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 1050)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 1050)

    # does training only have normal data?
    expect_false(any(i1 %nin% normal.inds))
  }
  expect_equal(sort(c(unique(unlist(rin$test.inds)))), 1:105)

  # OCCV instance is stochastic
  rin1 = makeResampleInstance(makeResampleDesc("OCCV", iters = 2L), oneclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("OCCV", iters = 2L), oneclass.task)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})


test_that("OCCV instance is stochastics", {
  rin = makeResampleInstance(makeResampleDesc("OCCV", iters = 3), oneclass.task)

  folds = rin$desc$iters
  expect_equal(folds, 3)
  normal.inds = which(oneclass.task$env$data$normal == oneclass.task$task.desc$positive)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
  }
  rin1 = makeResampleInstance(makeResampleDesc("OCCV", iters = 2L), oneclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("OCCV", iters = 2L), oneclass.task)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})

test_that("test.join works somehow", {
  lrn = makeLearner("oneclass.svm", predict.type = "prob")

  # check if test.join computes acc correctly
  mm = setAggregation(fp, test.join)
  r = resample(lrn, oneclass.task, cv2, measures = mm)
  rpred = getRRPredictions(r)
  expect_equal(as.numeric(r$aggr),
    sum(getPredictionTruth(rpred) != getPredictionResponse(rpred) & getPredictionResponse(rpred) == oneclass.positive))
})
