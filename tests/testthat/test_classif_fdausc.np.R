context("RLearner_classif_fdausc.np")

test_that("classif_fdausc.np behaves like original api", {
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
  a1 = suppressWarnings(fda.usc::classif.np(glearn, mlearn))

  # Fix bug in package. The changed slot looks different when called with
  # `fda.usc::classif.np()` than just `classif.np()`
  a1$C[[1]] = quote(classif.np)

  # newdat = list("x"=mtest)
  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn
  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = gtest

  lrn = makeLearner("classif.fdausc.np")
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")
  # suppressing "executing %dopar% sequentially: no parallel backend registered"
  m = suppressWarnings(train(lrn, task))
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)

  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))
})
