context("resample")

test_that("resample", {
  rin1 = makeResampleInstance(makeResampleDesc("Bootstrap", iters = 4), task = multiclass.task)
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters = 7), task = multiclass.task)
  rin3 = makeResampleInstance(makeResampleDesc("Subsample", iters = 2), task = multiclass.task)

  lrn = makeLearner("classif.lda")
  r1 = resample(lrn, multiclass.task, rin1)
  r2 = resample(lrn, multiclass.task, rin2)
  r3 = resample(lrn, multiclass.task, rin3)

  p1 = r1$pred
  p2 = r2$pred
  p3 = r3$pred

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
  # test printer
  expect_output(print(r1), "Resample Result")

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
  td = getTaskDesc(binaryclass.task)
  f1 = factor(rep(td$positive, cv.i$size), levels = td$class.levels)
  expect_equal(rf3$data$response, f1)
  f2 = factor(rep(td$negative, cv.i$size), levels = td$class.levels)
  expect_equal(rf4$data$response, f2)

  ct = makeClassifTask(data = iris[, c("Species", "Petal.Width")], target = "Species")
  fit = resample(lrn1, ct, makeResampleDesc("CV", iters = 2))

  expect_error(resample("classif.rpart", multiclass.task, makeResampleDesc("Holdout"),
    measures = list()), "length >= 1")
})

test_that("resampling, predicting train set works", {
  rdesc = makeResampleDesc("CV", iters = 2, predict = "train")
  lrn = makeLearner("classif.rpart")
  expect_error(resample(lrn, multiclass.task, rdesc), "not compatible with resampling")

  rdesc = makeResampleDesc("CV", iters = 2, predict = "train")
  lrn = makeLearner("classif.rpart")
  m = setAggregation(mmce, train.mean)
  r = resample(lrn, multiclass.task, rdesc, measures = m)
  expect_false(is.na(r$aggr["mmce.train.mean"]))
  expect_false(anyNA(r$pred$time))
  expect_false(is.null(r$pred$predict.type))
  expect_false(is.null(r$pred$threshold))
  expect_equal(getTaskDesc(multiclass.task), r$pred$task.desc)

  rdesc = makeResampleDesc("CV", iters = 2, predict = "both")
  lrn = makeLearner("classif.rpart")
  m1 = setAggregation(mmce, train.mean)
  m2 = setAggregation(mmce, test.mean)
  r = resample(lrn, multiclass.task, rdesc, measures = list(m1, m2))
  expect_false(is.na(r$aggr["mmce.train.mean"]))
  expect_false(is.na(r$aggr["mmce.test.mean"]))
  expect_false(anyNA(r$pred$time))
  expect_false(is.null(r$pred$predict.type))
  expect_false(is.null(r$pred$threshold))
  expect_equal(getTaskDesc(multiclass.task), r$pred$task.desc)
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

# issue #668
test_that("resample has error messages when prediction fails", {
  on.learner.error.saved = getMlrOptions()$on.learner.error
  on.learner.warning.saved = getMlrOptions()$on.learner.warning
  configureMlr(on.learner.error = "quiet")
  configureMlr(on.learner.warning = "quiet")

  lrn = makeLearner("classif.knn")
  lrn$properties = c(lrn$properties, "missings")

  task = makeClassifTask("test", data = Sonar, target = "Class")
  task$env$data$V1[1:2] = NA
  r = crossval(lrn, task)
  expect_false(all(is.na(r$err.msgs$predict)))

  configureMlr(on.learner.error = on.learner.error.saved)
  configureMlr(on.learner.warning = on.learner.warning.saved)
})

test_that("resample is extended by an additional measure", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")

  # check if it works with test, both and train
  predict = c("train", "test", "both")
  # check if it works with different aggregation methods
  aggr = list(test.mean, test.median, test.sd, test.range, test.join)
  for (a in aggr) {
    for (p in predict) {
      rdesc = makeResampleDesc("CV", iter = 3, predict = p)
      measures = list(mmce, ber, auc, brier)
      # set aggregation method
      measures = lapply(measures, setAggregation, a)
      if (p == "train") measures = lapply(measures, setAggregation, train.mean)
      # create ResampleResult with all measures
      res.all = resample(lrn, binaryclass.task, rdesc, measures)
      # create ResampleResult with one measure and add the other ones afterwards
      res = resample(lrn, binaryclass.task, rdesc, measures[[1]])
      res.add = addRRMeasure(res, measures[-1])

      # check if both ResampleResult objects are equal
      expect_equal(res.all$measures.train, res.add$measures.train)
      expect_equal(res.all$measures.test, res.add$measures.test)
      expect_equal(res.all$aggr, res.add$aggr)
    }
  }

  # keep.pred must be TRUE
  res = resample(lrn, binaryclass.task, cv3, mmce, keep.pred = FALSE)
  expect_error(addRRMeasure(res, auc), "keep.pred")
})

test_that("resample printer respects show.info", {
  show.info.saved = getMlrOptions()$show.info
  lrn = makeLearner("regr.lm")

  configureMlr(show.info = TRUE)
  expect_message(resample(lrn, bh.task, cv10, list(mape, medae, mse)))

  configureMlr(show.info = FALSE)
  expect_silent(resample(lrn, bh.task, cv10, list(mape, medae, mse)))

  configureMlr(show.info = show.info.saved)
})

test_that("resample drops unseen factors in predict data set", {
  data = data.frame(a = c("a", "b", "a", "b", "a", "c"),
    b = c(1, 1, 2, 2, 2, 1),
    trg = c("a", "b", "a", "b", "a", "b"))
  task = makeClassifTask("unseen.factors", data, "trg")
  resinst = makeResampleInstance("Holdout", task)
  resinst$train.inds[[1]] = 1:4
  resinst$test.inds[[1]] = 5:6

  lrn = makeLearner("classif.logreg", fix.factors.prediction = FALSE)
  model = train(lrn, subsetTask(task, 1:4, features = getTaskFeatureNames(task)))
  expect_error(predict(model, subsetTask(task, 5:6)), "factor a has new levels c")
  expect_error(resample(lrn, task, resinst), "factor a has new levels c")

  lrn = makeLearner("classif.logreg", fix.factors.prediction = TRUE)
  model = train(lrn, subsetTask(task, 1:4))
  predict(model, subsetTask(task, 5:6))
  resample(lrn, task, resinst)
})
