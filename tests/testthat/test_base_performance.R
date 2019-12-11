context("performance")

test_that("performance", {
  res = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.rpart")
  rf = resample(lrn, task = binaryclass.task, resampling = res,
    measures = list(acc))
  expect_true(all(rf$aggr > 0))

  res = makeResampleDesc("Bootstrap", iters = 3L)
  rf = resample(lrn, task = binaryclass.task, resampling = res,
    measures = list(acc))
  expect_true(all(rf$aggr > 0))

  m = setAggregation(acc, test.median)
  rf = resample(lrn, task = binaryclass.task, resampling = res, measures = m)
  expect_true(all(rf$aggr > 0))

  # custom measure
  res = makeResampleDesc("CV", iters = 3)
  mymeasure = makeMeasure(id = "mym", minimize = TRUE,
    properties = c("classif", "classif.multi", "predtype.response"),
    fun = function(task, model, pred, feats, extra.args) {
      # normal test error
      e1 = mean(pred$data$truth != pred$data$response)
      # we do this manually
      id = pred$data$id
      t2 = getTaskTargets(task)[id]
      e2 = mean(t2 != pred$data$response)
      expect_equal(e1, e2)
      0
    })
  r = resample(lrn, task = binaryclass.task, resampling = res,
    measures = mymeasure)
  expect_true(r$aggr >= 0)

  perf = performance(r$pred, measures = mymeasure, task = binaryclass.task)
  expect_true(perf >= 0)

  # multiple measures as list
  res = performance(r$pred, measures = list(ber, acc, tp),
    task = binaryclass.task)
  expect_true(!any(is.na(res)))
  expect_true(length(res) == 3)
  expect_equal(names(res), c("ber", "acc", "tp"))

  # custom measure
  mymeasure = makeCustomResampledMeasure(measure.id = "mym",
    aggregation.id = "train.mean",
    properties = c("classif", "predtype.response"),
    fun = function(task, group, pred, feats, extra.args) {
      mean(pred$data$truth != pred$data$response)
    })
  rdesc = makeResampleDesc("Holdout")
  r = resample(lrn, binaryclass.task, rdesc, measures = list(mmce, mymeasure))
  expect_equal(as.numeric(r$aggr["mmce.test.mean"]), as.numeric(r$aggr["mym.train.mean"]))
})

test_that("performance is NA if 'on.measure.not.applicable' is not 'stop'", {
  default = getMlrOption("on.measure.not.applicable")
  vals = c("quiet", "warn", "stop")
  for (i in vals) {
    configureMlr(on.measure.not.applicable = i)
    lrn = makeLearner("classif.qda", predict.type = "response")
    mod = train(lrn, binaryclass.task)
    pred = predict(mod, binaryclass.task)
    if (i == "quiet") {
      expect_equal(unname(performance(pred, auc)), NA_real_)
      # does this also work with benchmark?
      b = benchmark(lrn, binaryclass.task, measures = list(acc, auc),
        resamplings = cv5)
      expect_true(any(is.na(as.data.frame(b)$auc)))
      expect_false(any(is.na(as.data.frame(b)$acc)))
    } else if (i == "warn") {
      expect_warning(expect_equal(unname(performance(pred, auc)), NA_real_))
      # does this also work with benchmark?
      expect_warning({
        b = benchmark(lrn, binaryclass.task, measures = list(acc, auc),
          resamplings = cv5)
      })
      expect_true(any(is.na(as.data.frame(b)$auc)))
      expect_false(any(is.na(as.data.frame(b)$acc)))
    } else {
      expect_error(performance(pred, auc))
      # does this also work with benchmark?
      expect_error(benchmark(lrn.list, binaryclass.task,
        measures = list(acc, auc),
        resamplings = cv3))
    }
  }
  configureMlr(on.measure.not.applicable = default)
})

test_that("performance checks for missing truth col", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  m = train(lrn, binaryclass.task)
  test.x = getTaskData(binaryclass.task, target.extra = TRUE)$data
  pred = predict(m, newdata = test.x)

  expect_error(performance(pred, measures = mmce), "need to have a 'truth' col")
})

test_that("performance checks for req prob type", {
  lrn = makeLearner("classif.rpart")
  expect_error(holdout(lrn, binaryclass.task, measures = auc),
    "predict type to be: 'prob'")
})

test_that("performance works with ResamplePrediction", {
  lrn = makeLearner("classif.lda", predict.type = "prob")
  res = makeResampleDesc("Bootstrap", iters = 5L, predict = "both")
  rf = resample(lrn, task = binaryclass.task, resampling = res, mmce)
  expect_true(rf$aggr > 0)
  expect_true(rf$aggr < 1)

  perf = performance(rf$pred)
  expect_true(perf > 0)
  expect_true(perf < 1)

  # FIXME: names for measures are different for aggregated measures, which we
  # currently don't do because it breaks other stuff
  rf$aggr = setNames(rf$aggr, names(perf))
  expect_equal(rf$aggr, perf)
})
