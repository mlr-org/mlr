context("FDA_extactFeatures")
test_that("extractFDAFeatures", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMinMax())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_integer(nrow(df), lower = 129, upper = 129)
  expect_integer(ncol(df), lower = 5, upper = 5)
  expect_subset(colnames(df), c("UVVIS.mean", "NIR.min", "NIR.max", "heatan", "h20"))
})

test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1:30)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR]")
})


test_that("Wrong methods yield errors", {
  t = subsetTask(fuelsubset.task, subset = 1:2)

  wrng1 = function() {
    lrn = function(data, target, cols, vals = NULL) {1}
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn)
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng1())),
    "feat.method needs to return")


  wrng2 = function() {
    lrn = function(data) {data[, 1]}
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn)
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng2())),
    "Must have formal arguments")

  wrng3 = function() {
    lrn = function(data, target, cols, vals = NULL) {data.frame(1)}
    makeExtractFDAFeatMethod(z = lrn, rz = lrn)
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng3())),
    "unused arguments")
})

test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1:30)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR]")
})
test_that("extractFDAFeaturesDesc", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMinMax())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check desc
  expect_is(t$desc, "extractFDAFeatDesc")
  expect_subset(t$desc$coln, c(getTaskFeatureNames(fuelsubset.task),
    getTaskTargetNames(fuelsubset.task)))
  expect_subset(t$desc$target, getTaskTargetNames(fuelsubset.task))
  expect_character(unique(t$desc$colclasses), pattern = "numeric")
  expect_subset(unlist(t$desc$fd.features),
    unlist(getTaskDesc(fuelsubset.task)$fd.features))
  expect_subset(unlist(t$desc$fd.grids), unlist(getTaskDesc(fuelsubset.task)$fd.grids))
  expect_list(t$desc$extractFDAFeat)
  expect_list(t$desc$extractFDAFeat$UVVIS$args)
  expect_function(t$desc$extractFDAFeat$UVVIS$reextract)
  expect_list(t$desc$extractFDAFeat$NIR$args)
  expect_function(t$desc$extractFDAFeat$NIR$reextract)
})

test_that("extractFDAFeatures task equal data.frame", {
  # check data.frame output equal to task's data output
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1" = extractFDAFourier(trafo.coeff = "amplitude"))
  t2 = extractFDAFeatures(gp.subset, feat.methods = fm)
  gp.desc = getTaskDesc(gp.subset)
  gp.grids = gp.desc$fd.grids
  gp.feats = gp.desc$fd.features
  t3 = extractFDAFeatures(getTaskData(gp.subset), target = "X1", feat.methods = fm,
    fd.features =  gp.feats, fd.grids = gp.grids)
  expect_identical(getTaskData(t2$task), t3$data)
  expect_equal(t2$desc, t3$desc)
  expect_equal(t2$desc$extractFDAFeat$fd1$arg$trafo.coeff, "amplitude")

  expect_error(extractFDAFeatures(gp.subset, feat.methods = list("fd1" = extractFDAFourier(),
    "fd2" = extractFDAMean())), regexp = "Must be a subset of")
})

test_that("reExtractFDAFeatures", {
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reExtractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})

test_that("extract reExtract feat.methods all", {
  fm2 = list("all" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(fuelsubset.task, feat.methods = fm2)
  t4 = reExtractFDAFeatures(fuelsubset.task, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})

test_that("extract and reExtract Wavelets", {
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1" = extractFDAWavelets(filter = "haar", boundary = "reflection"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reExtractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})
