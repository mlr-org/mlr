context("resample_ocholdout")

test_that("OCholdoutrequire task", {
  expect_error(makeResampleInstance(makeResampleDesc("OCHoldout")), "For resampling for oneclass-classification 'task' must be supplied")
  rdesc = makeResampleDesc("OCHoldout")
  expect_error(resample("oneclass.svm",  resamplin = rdesc, show.info = TRUE), "argument \"task\" is missing, with no default")
})

test_that("OCholdout instance works", {
  rin = makeResampleInstance(makeResampleDesc("OCHoldout", split = 0.25), oneclass.task)
  expect_equal(rin$size, nrow(getTaskData(oneclass.task)))
  expect_equal(rin$desc$iters, 1)
  expect_equal(length(rin$train.inds), 1)
  expect_equal(length(rin$test.inds), 1)
  label = getTaskTargets(oneclass.task)
  size = length(label)
  normal.inds = which(label == oneclass.task$task.desc$negative)
  normal.size = length(normal.inds)
  expect_equal(length(rin$train.inds[[1]]), floor(0.25 * size))
  expect_equal(length(rin$test.inds[[1]]), size - floor(0.25 * size))

  # does training only have normal data?
  expect_false(any(length(rin$train.inds[[1]]) %nin% normal.inds))
})

test_that("holdout test.join works somehow", {
  lrn = makeLearner("oneclass.svm", predict.type = "prob")

  # for holdout test.join and test.mean should be the same
  rin = makeResampleDesc("OCHoldout")
  mm = list(setAggregation(auc, test.join), auc)
  r = resample(lrn, oneclass.task, rin, measures = mm)
  expect_equal(as.integer(diff(r$aggr)), 0)

  mm = list(setAggregation(fn, test.join), fn)
  r = resample(lrn, oneclass.task, rin, measures = mm)
  rpred = getRRPredictions(r)
  expect_equal(as.numeric(r$aggr[1]),
    sum(getPredictionTruth(rpred) != getPredictionResponse(rpred) & getPredictionResponse(rpred) == oneclass.negative))
})
