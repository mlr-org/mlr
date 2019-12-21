test_that("test createSpatialResamplingPlots() creates 10 ggplot objects", {

  # take more reps to see if the restriction on two reps works
  rdesc = makeResampleDesc("SpRepCV", folds = 5, reps = 3)
  r = resample(makeLearner("classif.qda"), spatial.task, rdesc)

  plots = createSpatialResamplingPlots(spatial.task, r, crs = 32717,
    repetitions = 2, x.axis.breaks = c(-79.065, -79.085),
    y.axis.breaks = c(-3.970, -4))

  expect_class(plots[[1]][[1]], "gg")
  expect_length(plots[[1]], 10)
})
