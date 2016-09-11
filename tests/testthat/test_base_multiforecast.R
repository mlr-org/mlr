context("forecast")

test_that("forecast regression task", {
  fc = mfcregr.task
  expect_equal(getTaskTargetNames(fc), colnames(getTaskData(mfcregr.task)))
  y = getTaskTargets(fc)
  expect_true(is.data.frame(y))
  expect_true(!is.null(colnames(y)))
})

test_that("Multivariate Forecast learning", {
  lrn = makeLearner("mfcregr.BigVAR", par.vals = list(p = 2, struct = "Basic", gran = c(2, 4),
                    h = 1, n.ahead = 1))

  # train predict eval
  mod = train(lrn, mfcregr.task)
  pred = predict(mod, newdata = mfcregr.test[, seq_len(4), drop = FALSE])
  p = performance(pred, task = mfcregr.task)
  expect_true(!is.na(p))
  # resample
  lrn = makeLearner("mfcregr.BigVAR", par.vals = list(p = 2, struct = "Basic", gran = c(2, 4),
                    h = 3, n.ahead = 3))
  r = holdout(lrn, task = mfcregr.task, split = .99)
  expect_true(!is.na(r$aggr))
})

# Need to test model multiplexer
