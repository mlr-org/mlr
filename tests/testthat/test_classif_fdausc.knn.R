context("Rlearner_classif_fdausc.knn")

test_that("classif_fdausc.knn behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]

  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index, ]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  # suppressing "executing %dopar% sequentially: no parallel backend registered"
  a1 = suppressWarnings(fda.usc::classif.knn(glearn, mlearn, par.CV = list(trim = 0.5)))
  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn

  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = gtest

  lrn = makeLearner("classif.fdausc.knn", par.vals = list(knn = 3L, trim = 0.5))
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")
  m = train(lrn, task)
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)

  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))
})

test_that("predicttype prob for fda.usc", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("classif.fdausc.knn", par.vals = list(knn = 1L, trim = 0.5), predict.type = "prob")

  m = train(lrn, fda.binary.gp.task)
  cp = predict(m, newdata = getTaskData(fda.binary.gp.task, target.extra = TRUE, functionals.as = "matrix")$data)
  expect_equal(class(cp)[1], "PredictionClassif")
})

test_that("resampling fdausc.knn", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("classif.fdausc.knn", par.vals = list(knn = 1L, trim = 0.5), predict.type = "prob")

  r = resample(lrn, fda.binary.gp.task.small, cv2)
  expect_class(r, "ResampleResult")
})

test_that("resampling fdausc.knn", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("classif.fdausc.knn", par.vals = list(knn = 1L, metric = "metric.lp"), predict.type = "prob")

  r = resample(lrn, fda.binary.gp.task.small, hout)
  expect_class(r, "ResampleResult")
})
