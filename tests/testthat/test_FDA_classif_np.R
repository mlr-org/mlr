context("FDA_classif_np")

test_that("FDA_classif_np", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme)
  mlearn = phoneme[["learn"]]
  glearn = phoneme[["classlearn"]]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.np(glearn, mlearn)
  # restructure internal function call (language-object)
  a1$C[[1]] = quote(classif.np)
  #newdat = list("x"=mtest)
  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  ph = as.data.frame(mlearn$data)
  ph[,"label"] = glearn

  lrn = makeLearner("fdaclassif.np")
  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m = try(train(lrn, task))
  # restructure internal function call (language-object)
  cp = predict(object = m, newdata = as.data.frame(mtest$data))
  cp = unlist(cp$data, use.names = FALSE)

  cp2 = predict(m, newdata = as.data.frame(mlearn$data))
  cp2 = unlist(cp2$data, use.names = FALSE)

  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))

})
