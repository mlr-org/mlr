context("performance")

test_that("performance", {
  res = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.rpart")
  rf = resample(lrn, task = binaryclass.task, resampling = res, measures = list(acc, timeboth))

  res = makeResampleDesc("Bootstrap", iters = 3)
  rf = resample(lrn, task = binaryclass.task, resampling = res, measures = list(acc, timeboth))
  m = setAggregation(acc, test.median)
  rf = resample(lrn, task = binaryclass.task, resampling = res, measures = m)

  # custom measure
  res = makeResampleDesc("CV", iters = 3)
  r = resample(lrn, task = binaryclass.task, resampling = res)

  mymeasure = makeMeasure(id = "mym", minimize = TRUE, properties = c("classif", "classif.multi", "predtype.response"),
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

  performance(r$pred, measures = mymeasure, task = binaryclass.task)

  # multiple measures as list
  res = performance(r$pred, measures = list(ber, acc, tp), task = binaryclass.task)
  expect_true(!any(is.na(res)))
  expect_true(length(res) == 3)
  expect_equal(names(res), c("ber", "acc", "tp"))

  # custom measure

  mymeasure = makeCustomResampledMeasure(id = "mym", fun = function(task, group, pred, feats, extra.args) {
    mean(pred$data$truth != pred$data$response)
  })
  rdesc = makeResampleDesc("Holdout")
  r = resample(lrn, binaryclass.task, rdesc, measures = list(mmce, mymeasure))
  expect_equal(as.numeric(r$aggr["mmce.test.mean"]), as.numeric(r$aggr["custom.mym"]))
})
