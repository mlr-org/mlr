context("FDA_classif_knn")

test_that("FDA_classif_knn behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index, ]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.knn(glearn, mlearn, par.CV = list(trim = 0.5))
  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn

  lrn = makeLearner("fdaclassif.knn", par.vals = list(knn = 1L, trim = 0.5))
  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)
  cp = predict(m, newdata = as.data.frame(mtest$data))
  cp = unlist(cp$data, use.names = FALSE)

  cp2 = predict(m, newdata = as.data.frame(mlearn$data))
  cp2 = unlist(cp2$data, use.names = FALSE)
  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))

})
