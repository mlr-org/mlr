context("resample_cv")

test_that("getResamplingIndices works with getTuneResult", {

  if (getRversion() > "3.5.3") {
    suppressWarnings(RNGversion("3.5.0"))
  }
  set.seed(getOption("mlr.debug.seed"))

  task = makeClassifTask(data = iris, target = "Species")
  lrn = makeLearner("classif.rpart")
  # stupid mini grid
  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(0.05, 0.1)),
    makeDiscreteParam("minsplit", values = c(10, 20))
  )
  ctrl = makeTuneControlGrid()
  inner = makeResampleDesc("Holdout")
  outer = makeResampleDesc("CV", iters = 2)
  lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
  mod = train(lrn, task)
  print(getTuneResult(mod))

  # nested resampling for evaluation
  r = resample(lrn, task, outer, extract = getTuneResult)

  # check outer indices
  expect_equal(length(getResamplingIndices(r)$train.inds[[1]]), 75)

  # check if inner test.inds are retrieved correctly
  expect_length(unique(unlist(getResamplingIndices(r, inner = TRUE)[[1]]$test.inds)), 25)
})

test_that("getResamplingIndices works with getFeatSelResult", {

  if (getRversion() > "3.5.3") {
    suppressWarnings(RNGversion("3.5.0"))
  }
  set.seed(getOption("mlr.debug.seed"))

  outer = makeResampleDesc("CV", iters = 2L)
  inner = makeResampleDesc("Holdout")

  lrn1 = makeLearner("classif.rpart")
  ctrl = makeFeatSelControlRandom(maxit = 3)
  lrn2 = makeFeatSelWrapper(lrn1, resampling = inner, control = ctrl)

  r = resample(lrn2, multiclass.task, outer, extract = function(model) {

    getFeatSelResult(model)
  })

  # check outer indices
  expect_equal(length(getResamplingIndices(r)$train.inds[[1]]), 75)

  # check if inner test.inds are retrieved correctly
  expect_length(unique(unlist(getResamplingIndices(r, inner = TRUE)[[1]]$test.inds)), 25)
})

test_that("getResamplingIndices(inner = TRUE) correctly translates the inner inds to indices of the task", {

  if (getRversion() > "3.5.3") {
    suppressWarnings(RNGversion("3.5.0"))
  }
  set.seed(getOption("mlr.debug.seed"))

  # this test is from "test_base_fixed_indices_cv.R"
  df = multiclass.df
  fixed = as.factor(rep(1:5, rep(30, 5)))
  ct = makeClassifTask(target = multiclass.target, data = df, blocking = fixed)
  lrn = makeLearner("classif.ranger")
  ctrl = makeTuneControlRandom(maxit = 10)
  ps = makeParamSet(makeIntegerParam("num.trees", lower = 50, upper = 100))
  inner = makeResampleDesc("CV", fixed = TRUE)
  outer = makeResampleDesc("CV", fixed = TRUE)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE)
  p = resample(tune_wrapper, ct, outer, show.info = FALSE,
    extract = getTuneResult)

  inner_inds = getResamplingIndices(p, inner = TRUE)

  # to test we expect that any inner fold contains indices that exceed $obs - (obs / nfolds)$ = 150 - 30 = 120
  # 120 is the max index number that is used in the inner resampling (in the case of 150 obs and 5 folds) because we have one fold less than in the outer level
  inds = sort(inner_inds[[2]][["test.inds"]][[1]])

  expect_equal(length(inds[inds > 120]), 30)
})
