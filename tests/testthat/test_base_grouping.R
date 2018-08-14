context("grouping")

test_that("grouping in single resampling", {
  df = multiclass.df
  grouping = as.factor(rep(1:30, 5))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df,
    blocking = grouping)

  # test blocking in single resample
  lrn = makeLearner("classif.lda")
  rdesc = makeResampleDesc("CV", iters = 30, grouping = TRUE)
  p = resample(lrn, ct, rdesc)$pred

  # check if all test.inds are unique
  expect_length(unique(unlist(p$instance$test.inds, use.names = FALSE)), 150)

  # warning for wrong iter count
  rdesc = makeResampleDesc("CV", iters = 2)
  expect_warning(makeResampleInstance(rdesc, ct))
})

test_that("grouping in nested resampling", {
  df = multiclass.df
  #df$Species = NULL
  grouping = as.factor(rep(1:5, rep(30, 5)))
  ct = makeClassifTask(target = multiclass.target, data = df,
   blocking = grouping)

  # test grouping in nested resampling
  lrn = makeLearner("classif.lda")
  ctrl <- makeTuneControlRandom(maxit = 2)
  ps <- makeParamSet(makeNumericParam("nu", lower = 2, upper = 20))
  inner = makeResampleDesc("CV", iters = 4, grouping = TRUE)
  outer = makeResampleDesc("CV", iters = 5, grouping = TRUE)
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

  # expect warning if the level of the inner inds is not reduced by one
  inner = makeResampleDesc("CV", iters = 5, grouping = TRUE)
  outer = makeResampleDesc("CV", iters = 5, grouping = TRUE)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE)

  warns = capture_warnings(resample(tune_wrapper, ct, outer,
    show.info = FALSE, extract = getTuneResult))

  expect_match(warns[1],
   "iters (5) is not equal to length of blocking levels (4)!", fixed = TRUE)
})
