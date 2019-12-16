context("RLearner_classif_fdausc.glm")

test_that("classif_fdausc.glm behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn = phoneme[["learn"]]
  mlearn$data = mlearn$data[index, ]
  glearn = phoneme[["classlearn"]][index]
  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]

  dataf = data.frame(glearn)
  dat = list("df" = dataf, "x" = mlearn)
  # glm sometimes does not converge, we dont want to see that
  a1 = suppressWarnings(fda.usc::classif.glm(glearn ~ x, data = dat))

  # Fix bug in package. The changed slot looks different when called with
  # `fda.usc::lassif.glm()` than just `classif.glm()`
  a1$C[[1]] = quote(classif.glm)

  p1 = predict(a1, list("x" = mtest))
  p2 = predict(a1, list("x" = mlearn))

  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn
  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = gtest

  lrn = makeLearner("classif.fdausc.glm")
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")

  # glm sometimes does not converge, we dont want to see that
  m = suppressWarnings(train(lrn, task))
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)

  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))
})
