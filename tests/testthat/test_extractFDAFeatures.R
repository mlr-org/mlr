context("FDA_extactFeatures")
test_that("extractFDAFeatures", {
  methods = list("UVVIS" = extractFDAMean(), "NIR" = extractFDAMinMax())
  t = extractFDAFeatures(fuelSubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_integer(nrow(df), lower = c(129), upper = c(129))
  expect_integer(ncol(df), lower = c(5), upper = c(5))
  expect_subset(colnames(df), c("UVVIS", "NIR.min", "NIR.max", "V366", "heatan"))

  # check desc
  expect_is(t$desc, "extractFDAFeatDesc")
  expect_subset(t$desc$coln, c(getTaskFeatureNames(fuelSubset.task),
    getTaskTargetNames(fuelSubset.task)))
  expect_subset(t$desc$target, getTaskTargetNames(fuelSubset.task))
  expect_character(unique(t$desc$colclasses), pattern = "numeric")
  expect_subset(unlist(t$desc$fd.features),
    unlist(getTaskDesc(fuelSubset.task)$fd.features))
  expect_subset(unlist(t$desc$fd.grids), unlist(getTaskDesc(fuelSubset.task)$fd.grids))
  expect_list(t$desc$extractFDAFeat)
  expect_list(t$desc$extractFDAFeat$UVVIS$args)
  expect_function(t$desc$extractFDAFeat$UVVIS$reextract)
  expect_list(t$desc$extractFDAFeat$NIR$args)
  expect_function(t$desc$extractFDAFeat$NIR$reextract)

  # check for data.frame produces output equal to task
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1"= extractFDAFourier(trafo.coeff = "amplitude"))
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
  # check for data.frame produces output equal to task
  gp.subset = subsetTask(gunpoint.task, features = c(1:10))
  fm = list("fd1"= extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reExtractFDAFeatures(gp.subset, t3$desc)

})
