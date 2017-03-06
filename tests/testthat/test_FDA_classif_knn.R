context("FDA_classif_knn")

test_that("FDA_classif_knn", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  glearn = phoneme[["classlearn"]]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.knn(glearn, mlearn)
  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[,"label"] = glearn

  lrn = makeLearner("fdaclassif.knn")
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
