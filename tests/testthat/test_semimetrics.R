context("FDA_classif_knn")

test_that("semimetrics.mlr.basis work exactly the same as fda.usc::semimetrics.basis", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  # Use only 10 obs. for 5 classes as the metric functions are really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index,]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]

  # one data set
  sem.mlr = semimetric.mlr.basis(mlearn)
  sem.fda.usc = semimetric.basis(mlearn)
  # delete attributes, because calling structure is different
  attributes(sem.mlr) = NULL
  attributes(sem.fda.usc) = NULL

  expect_equal(sem.mlr, sem.fda.usc)

  # two data sets
  sem.mlr.test = semimetric.mlr.basis(mlearn, mtest)
  sem.fda.usc.test = semimetric.basis(mlearn, mtest)
  # delete attributes, because calling structure is different
  attributes(sem.mlr.test) = NULL
  attributes(sem.fda.usc.test) = NULL

  expect_equal(sem.mlr.test, sem.fda.usc.test)

})

test_that("FDA_classif_knn behaves like original api for the newly implemented semimetrics", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index,]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]

  # fda.usc implementation
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.knn(glearn, mlearn, knn = 1L,
                            metric = semimetric.basis,
                            par.CV = list(trim = 0.5))

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[,"label"] = glearn

  # mlr interface
  lrn = makeLearner("fdaclassif.knn",
                    metric = "semimetric.mlr.basis",
                    par.vals = list(knn = 1L, trim = 0.5))
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



