context("resample")

test_that("resample", {
  rin1 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 4), task = multiclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters = 7), task = multiclass.task)
  rin3 = makeResampleInstance(makeResampleDesc("Subsample", iters = 2), task = multiclass.task)

  lrn = makeLearner("classif.lda")
  p1 = resample(lrn, multiclass.task, rin1)$pred
  p2 = resample(lrn, multiclass.task, rin2)$pred
  p3 = resample(lrn, multiclass.task, rin3)$pred

  inds = Reduce(c, rin1$test.inds)
  y = getTaskTargets(multiclass.task)[inds]
  expect_equal(p1$data$id, inds)
  expect_equal(p1$data$truth, y)
  inds = Reduce(c, rin2$test.inds)
  y = getTaskTargets(multiclass.task)[inds]
  expect_equal(p2$data$id, inds)
  expect_equal(p2$data$truth, y)
  inds = Reduce(c, rin3$test.inds)
  y = getTaskTargets(multiclass.task)[inds]
  expect_equal(p3$data$id, inds)
  expect_equal(p3$data$truth, y)

  cv.i = makeResampleInstance(makeResampleDesc("CV", iters = 3), binaryclass.task)

  lrn1 = makeLearner("classif.lda")
  lrn2 = makeLearner("classif.lda", predict.type = "prob")
  rf1 = resample(lrn1, binaryclass.task, cv.i)$pred
  rf2 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf3 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf3 = setThreshold(rf3, 0)
  rf4 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf4 = setThreshold(rf4, 1)

  expect_equal(rf1$data$response, rf2$data$response)
  td = getTaskDescription(binaryclass.task)
  f1 = factor(rep(td$positive, cv.i$size), levels = td$class.levels)
  expect_equal(rf3$data$response, f1)
  f2 = factor(rep(td$negative, cv.i$size), levels = td$class.levels)
  expect_equal(rf4$data$response, f2)

  ct = makeClassifTask(data = iris[,c("Species", "Petal.Width")], target = "Species")
  fit = resample(lrn1, ct, makeResampleDesc("CV", iters = 2))

  expect_error(resample("classif.rpart", multiclass.task, makeResampleDesc("Holdout"),
      measures = list()), "length >= 1")
})

test_that("resampling, predicting train set works", {
  rdesc = makeResampleDesc("CV", iters = 2, predict = "train")
  lrn = makeLearner("classif.rpart")
  r = resample(lrn, multiclass.task, rdesc)
  expect_true(as.logical(is.na(r$aggr["mmce.test.mean"])))

  rdesc = makeResampleDesc("CV", iters = 2, predict = "train")
  lrn = makeLearner("classif.rpart")
  m = setAggregation(mmce, train.mean)
  r = resample(lrn, multiclass.task, rdesc, measures = m)
  expect_true(!as.logical(is.na(r$aggr["mmce.train.mean"])))

  rdesc = makeResampleDesc("CV", iters = 2, predict = "both")
  lrn = makeLearner("classif.rpart")
  m1 = setAggregation(mmce, train.mean)
  m2 = setAggregation(mmce, test.mean)
  r = resample(lrn, multiclass.task, rdesc, measures = list(m1, m2))
  expect_true(!as.logical(is.na(r$aggr["mmce.train.mean"])))
  expect_true(!as.logical(is.na(r$aggr["mmce.test.mean"])))

})


test_that("ResampleInstance can bew created from string", {
  rin = makeResampleInstance("CV", size = 100)
  expect_is(rin$desc, "CVDesc")
  expect_equal(rin$size, 100)
  expect_equal(rin$desc$iters, 10)

  rin = makeResampleInstance("CV", task = iris.task, iters = 17, stratify = TRUE)
  expect_is(rin$desc, "CVDesc")
  expect_equal(rin$size, 150)
  expect_equal(rin$desc$iters, 17)
})

test_that("resample checks constraints", {
  expect_error(makeResampleInstance("CV", iters = 20, size = 10), "more folds")
  expect_error(makeResampleInstance("RepCV", folds = 20, reps = 2L, size = 10), "more folds")
})

test_that("resample returns errors", {
  configureMlr(on.learner.error = "quiet")

  lrn = makeLearner("classif.__mlrmocklearners__2", alpha = 1)
  z = holdout(lrn, multiclass.task)
  expect_true(!is.na(z$aggr))
  expect_true(is.data.frame(z$err.msgs))
  expect_true(nrow(z$err.msgs) == 1L)
  expect_true(all(is.na(z$err.msgs$train)))
  expect_true(all(is.na(z$err.msgs$predict)))

  lrn = makeLearner("classif.__mlrmocklearners__2", alpha = 0)
  m = train(lrn, multiclass.task)
  expect_true(isFailureModel(m))
  z = crossval(lrn, multiclass.task, iters = 2L)
  expect_true(is.na(z$aggr))
  expect_true(is.data.frame(z$err.msgs))
  expect_true(nrow(z$err.msgs) == 2L)
  expect_true(all(!is.na(z$err.msgs$train)))
  expect_true(all(is.na(z$err.msgs$predict)))

  configureMlr(on.learner.error = "stop")
})
