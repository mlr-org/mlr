# check some basic stuff here, the rest is done in test_featsel_
context("selectFeatures basic")

test_that("selectFeatures", {
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.lda")

  ctrl = makeFeatSelControlRandom(maxit = 2L)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc, control = ctrl, show.info = FALSE)
  expect_equal(getOptPathLength(fr$opt.path), 2L)
  expect_equal(nrow(as.data.frame(fr$opt.path)), 2L)
  expect_equal(ncol(as.data.frame(fr$opt.path)), 9L)

  # test printing
  expect_output(print(ctrl), "Imputation value: <worst>")
  ctrl$impute.val = 10
  expect_output(print(ctrl), "Imputation value: 10")
  expect_output(print(fr), "mmce.test.mean")

  # check empty model
  ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 10)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc, control = ctrl, show.info = FALSE)
  expect_equal(fr$x, character(0))

  # check bits.to.features
  bns = c("b1", "b2")
  btf = function(x, task) {
    fns = getTaskFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(x)], init = character(0))
  }
  ctrl = makeFeatSelControlRandom(maxit = 2L)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc, bit.names = bns,
    bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol",
        "exec.time", "error.message")))
  expect_equal(nrow(df), 2L)
})

test_that("threshold tuning with feature selection", {
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  ctrl = makeFeatSelControlRandom(maxit = 2L, tune.threshold = TRUE,
    tune.threshold.args = list(nsub = 2L))
  fr = selectFeatures(lrn, task = binaryclass.task, resampling = rdesc, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(is.numeric(df$threshold) && !any(is.na(df$threshold)))
})


