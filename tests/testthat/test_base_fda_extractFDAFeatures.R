context("extactFDAFeatures")

test_that("extractFDAFeatures", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(),
    "NIR" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129L)
  expect_subset(colnames(df), c(paste0("NIR.phase.", seq_len(231)),
    paste0("UVVIS.multires.", seq_len(9)), "heatan", "h20"))
})

test_that("extractFeatures multiple times", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(),
    "UVVIS" = extractFDAFourier(),
    "NIR" = extractFDAMultiResFeatures())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_class(df, "data.frame")
  expect_true(nrow(df) == 129L)
  expect_true(ncol(df) == 154L)
  expect_subset(colnames(df), c("heatan", "h20", paste0("UVVIS.phase.",
    seq_len(134)),
  paste0("NIR.multires.", seq_len(9)), paste0("UVVIS.multires.",
    seq_len(9))))

  methods = list("all" = extractFDAMultiResFeatures(), "all" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_true(nrow(df) == 129L)
  expect_true(ncol(df) == 385L)
  expect_subset(colnames(df),
    c("heatan", "h20",
      paste0("UVVIS.multires.", seq_len(9)), paste0("NIR.multires.", seq_len(9)),
      paste0("UVVIS.phase.", seq_len(134)), paste0("NIR.phase.", seq_len(231))))
})

test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1:30)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR.phase]", all = TRUE)
})

test_that("Wrong methods yield errors", {
  t = subsetTask(fuelsubset.task, subset = 1:2)

  wrng1 = function() {
    lrn = function(data, target, col, vals = NULL) {
      1
    }
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
      par.set = makeParamSet())
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng1())),
    "feat.method needs to return")


  wrng2 = function() {
    lrn = function(data) {
      data[, 1]
    }
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn,
      par.set = makeParamSet())
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng2())),
    "Must have formal arguments")

  wrng3 = function() {
    lrn = function(data, target, col, vals = NULL) {
      data.frame(1)
    }
    makeExtractFDAFeatMethod(z = lrn, rz = lrn)
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng3())),
    "unused arguments")
})

test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR.phase]", all = TRUE)
})

test_that("extractFDAFeaturesDesc", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(),
    "NIR" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check desc
  expect_is(t$desc, "extractFDAFeatDesc")
  expect_subset(t$desc$coln, c(getTaskFeatureNames(fuelsubset.task),
    getTaskTargetNames(fuelsubset.task)))
  expect_subset(t$desc$target, getTaskTargetNames(fuelsubset.task))
  expect_subset(unique(t$desc$colclasses), choices = c("numeric", "matrix"))
  expect_list(t$desc$extractFDAFeat)
  expect_list(t$desc$extractFDAFeat$UVVIS$extractor.vals)
  expect_function(t$desc$extractFDAFeat$UVVIS$reextract)
  expect_list(t$desc$extractFDAFeat$NIR$extractor.vals)
  expect_function(t$desc$extractFDAFeat$NIR$reextract)
})

test_that("extractFDAFeatures task equal data.frame", {
  # check data.frame output equal to task's data output
  gp.subset = subsetTask(gunpoint.task, features = 1L)
  fm = list("fd" = extractFDAFourier(trafo.coeff = "amplitude"))
  t2 = extractFDAFeatures(gp.subset, feat.methods = fm)
  gp.desc = getTaskDesc(gp.subset)
  t3 = extractFDAFeatures(getTaskData(gp.subset, functionals.as = "matrix"),
    target = "X1", feat.methods = fm)
  expect_identical(getTaskData(t2$task), t3$data)
  expect_equal(t2$desc, t3$desc)
  expect_equal(t2$desc$extractFDAFeat$fd$extractor.vals$trafo.coeff, "amplitude")

  expect_error(extractFDAFeatures(gp.subset,
    feat.methods = list("fd" = extractFDAFourier(),
      "fd2" = extractFDAMultiResFeatures())), regexp = "Must be a subset of")
})

test_that("reextractFDAFeatures", {
  gp.subset = subsetTask(gunpoint.task, features = 1L)
  fm = list("fd" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})

test_that("extract reextract feat.methods all", {
  fm2 = list("all" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(fuelsubset.task, feat.methods = fm2)
  t4 = reextractFDAFeatures(fuelsubset.task, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})

test_that("extract and reextract have correct args", {
  lrn = makeExtractFDAFeatsWrapper("regr.rpart",
    feat.methods = list("all" = extractFDAFourier()))
  mod = train(setHyperPars(lrn, trafo.coeff = "amplitude"),
    subsetTask(fuelsubset.task, subset = 1:20))
  prd = predict(mod, subsetTask(fuelsubset.task, subset = 21:40))
  expect_equal(mod$learner.model$control$extractFDAFeat$UVVIS$extractor.vals$trafo.coeff,
    "amplitude")
  expect_equal(mod$learner.model$control$extractFDAFeat$NIR$extractor.vals$trafo.coeff,
    "amplitude")
})
