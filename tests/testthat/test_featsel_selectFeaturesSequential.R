context("selectFeaturesSequential")

# This used to cause an error. Now ensure it does not.
test_that("no crash with sffs", {
  p = mlbench::mlbench.waveform(1000)
  dataset = as.data.frame(p)
  dataset = droplevels(subset(dataset, classes != 3))

  m.ct = makeClassifTask(data = dataset, target = "classes")
  ctrl = makeFeatSelControlSequential(method = "sffs", maxit = NA,
    alpha = 0.001)
  m.l = makeLearner("classif.logreg", predict.type = "prob")
  inner = makeResampleDesc("Holdout", stratify = TRUE)
  lrn = makeFeatSelWrapper(m.l, resampling = inner, control = ctrl)
  outer = makeResampleDesc("CV", iters = 2, stratify = TRUE)
  # No error occurs
  expect_error(resample(lrn, m.ct, outer, extract = getFeatSelResult,
    measures = list(mlr::auc, mlr::acc, mlr::brier), models = TRUE), NA)
})
