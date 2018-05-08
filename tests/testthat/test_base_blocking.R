context("blocking")

test_that("blocking in single resampling", {
  df = multiclass.df
  b = as.factor(rep(1:30, 5))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df, blocking = b)
  expect_true(getTaskDesc(ct)$has.blocking)

  # test blocking in single resample
  lrn = makeLearner("classif.lda")
  rdesc = makeResampleDesc("Blocking")
  p = resample(lrn, ct, rdesc)$pred

  # check if all test.inds are unique
  expect_length(unique(unlist(p$instance$test.inds, use.names = FALSE)), 150)

  # warning for wrong iter count
  rdesc = makeResampleDesc("Blocking", iters = 2)
  expect_warning(resample(lrn, ct, rdesc))
})

test_that("blocking in nested resampling", {
  df = multiclass.df
  b = as.factor(rep(1:5, rep(30, 5)))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df, blocking = b)
  expect_true(getTaskDesc(ct)$has.blocking)

  # test blocking in nested resampling
  lrn = makeLearner("classif.lda")
  ctrl <- makeTuneControlRandom(maxit = 2)
  ps <- makeParamSet(makeNumericParam("nu", lower = 2, upper = 20))
  inner = makeResampleDesc("Blocking", iters = 2)
  # outer = makeResampleDesc("CV", iters = 5)
  outer = makeResampleDesc("Blocking", iters = 3)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl,
                                 show.info = FALSE)

  p = resample(tune_wrapper, ct, outer, show.info = FALSE, extract = getTuneResult)

  # check if all test.inds are unique
  expect_length(unique(unlist(p$pred$instance$test.inds, use.names = FALSE)), 150)

  # check if we have the correct number of tuning results
  expect_length(p$extract, 5)

})
