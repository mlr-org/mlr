context("FDA_classif_glm")

test_that("FDA_classif_glm", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  glearn = phoneme[["classlearn"]]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  dataf = data.frame(glearn)
  dat = list("df"=dataf,"x"=mlearn)
  # glm sometimes does not converge, we dont want to see that
  a1 = suppressWarnings(fda.usc::classif.glm(glearn~x, data = dat))
  # restructure internal function call (language-object)
  # FIXME: code looks strange here? the quote?
  a1$C[[1]] = quote(classif.glm)
  newdat = list("x"=mtest)
  p1 = predict(a1, newdat)

  ph = as.data.frame(mlearn$data)
  ph[,"label"] = glearn

  lrn = makeLearner("fdaclassif.glm")
  task = makeFDAClassifTask(data = ph, target = "label")
  # glm sometimes does not converge, we dont want to see that
  m = suppressWarnings(train(lrn, task))
  cp = predict(m, newdata = as.data.frame(mtest$data))
  cp = unlist(cp$data, use.names = FALSE)
  expect_equal(as.character(cp), as.character(p1))
})
