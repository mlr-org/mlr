context("fixed")

test_that("fixed in single resampling", {

  df = multiclass.df
  fixed = as.factor(rep(1:30, 5))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df,
    blocking = fixed)

  # test blocking in single resample
  lrn = makeLearner("classif.lda")
  rdesc = makeResampleDesc("CV", fixed = TRUE)
  p = resample(lrn, ct, rdesc)$pred

  # check if all test.inds are unique
  expect_length(unique(unlist(p$instance$test.inds, use.names = FALSE)), 150)
  # check if correct indices are together (one fold is enough)
  expect_equal(p$instance$test.inds[[1]], c(23, 53, 83, 113, 143))
})

test_that("fixed in nested resampling", {
  df = multiclass.df
  fixed_inds = as.factor(rep(1:5, rep(30, 5)))
  ct = makeClassifTask(target = multiclass.target, data = df,
    blocking = fixed_inds)

  # test fixed in nested resampling
  lrn = makeLearner("classif.lda")
  ctrl = makeTuneControlRandom(maxit = 2)
  ps = makeParamSet(makeNumericParam("nu", lower = 2, upper = 20))
  inner = makeResampleDesc("CV", iters = 4, fixed = TRUE)
  outer = makeResampleDesc("CV", iters = 5, fixed = TRUE)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE)

  p = resample(tune_wrapper, ct, outer, show.info = FALSE,
    extract = getTuneResult)

  # check if all outer test.inds are unique
  expect_length(unique(unlist(
    p$pred$instance$test.inds, use.names = FALSE)), 150)

  # check if all inner test.inds are unique
  # we only expect 120 since we tune on n-1 folds (n = count(outer folds))
  expect_length(unique(unlist(
    getResamplingIndices(p, inner = TRUE)[[1]]$test.inds)), 120)

  # check if we have the correct number of tuning results
  expect_length(p$extract, 5)

  # check that a combination of fixed and normal random sampling works
  inner = makeResampleDesc("CV", iters = 6)
  outer = makeResampleDesc("CV", fixed = TRUE)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE)
  p = resample(tune_wrapper, ct, outer, show.info = FALSE,
    extract = getTuneResult)
  expect_length(getResamplingIndices(p, inner = TRUE)[[1]][[1]], 6)
})
