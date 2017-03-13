context("FDA_extactFeatures")
test_that("extractFDAFeatures", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMinMax())
  t = extractFDAFeatures(fuelSubset.task, feat.methods = methods)
  # check output data
  expect_is(t$data, "data.frame")
  expect_integer(nrow(t$data)[1], lower = c(129), upper = c(129))
  expect_integer(ncol(t$data)[1], lower = c(3), upper = c(3))
  expect_subset(colnames(t$data), c("UVVIS", "NIR.min", "NIR.max"))

  # check desc
  expect_is(t$desc, "extractFDAFeatDesc")
  expect_subset(t$desc$coln, getTaskFeatureNames(fuelSubset.task))
  expect_subset(t$desc$target, getTaskTargetNames(fuelSubset.task))
  expect_character(unique(t$desc$colclasses), pattern = "numeric")
  expect_subset(unlist(t$desc$fd.features),
    unlist(getTaskDescription(fuelSubset.task)$fd.features))
  expect_subset(unlist(t$desc$fd.grids), unlist(getTaskDescription(fuelSubset.task)$fd.grids))
  expect_list(t$desc$extractFDAFeat$UVVIS$args)
  expect_numeric(t$desc$extractFDAFeat$UVVIS$feats)
  expect_function(t$desc$extractFDAFeat$UVVIS$reextract)
  expect_list(t$desc$extractFDAFeat$NIR$args)
  expect_is(t$desc$extractFDAFeat$NIR$feats, "data.frame")
  expect_function(t$desc$extractFDAFeat$NIR$reextract)

  # check for data.frame produces output equal to task
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1"= extractFDAFourier(trafo.coeff = "amplitude"))
  t2 = extractFDAFeatures(gp.subset, feat.methods = fm)
  gp.desc = getTaskDescription(gp.subset)
  gp.grids = gp.desc$fd.grids
  gp.feats = gp.desc$fd.features
  t3 = extractFDAFeatures(getTaskData(gp.subset), target = "X1", feat.methods = fm,
    fd.features =  gp.feats, fd.grids = gp.grids)
  expect_identical(t2$data, t3$data)
  expect_equal(t2$desc, t3$desc)
  expect_equal(t2$desc$extractFDAFeat$fd1$arg$trafo.coeff, "amplitude")

  expect_error(extractFDAFeatures(gp.subset, feat.methods = list("fd1" = extractFDAFourier(),
    "fd2" = extractFDAMean())), regexp = "Must be a subset of")
})

test_that("reExtractFDAFeatures", {
  # check for data.frame produces output equal to task
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1"= extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)

  t4 = reExtractFDAFeatures(gp.subset, t3$desc)
})
