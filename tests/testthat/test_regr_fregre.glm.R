context("RLearner_regr_fregre.glm")

test_that("regr_fregre.glm behaves like original api", {

  # create data-----------------------------------------------------------------

  requirePackagesOrSkip("fda.usc", default.method = "load")
  requirePackagesOrSkip("fda", default.method = "load")

  data("tecator", package = "fda.usc")

  index = c(1:70, 150:200)
  mlearn = tecator[["learn"]]
  mlearn$df = tecator$y[index, ]
  mlearn$absorb.fdata = tecator$absorp.fdata
  mlearn$absorb.fdata$data = mlearn$absorb.fdata$data[index, ]

  mtest = tecator[["test"]]
  mtest$df = tecator$y
  mtest$absorb.fdata = tecator$absorp.fdata$data

  mtest$absorb.fdata = tecator$absorp.fdata
  mtest$absorb.fdata$data = mtest$absorb.fdata$data

  a1 = fda.usc::fregre.glm(Fat ~ absorb.fdata, data = mlearn)

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)
  ph = as.data.frame(mlearn$absorb.fdata$data)
  ph[, "label"] = mlearn$df$Fat

  phtest = as.data.frame(mtest$absorb.fdata$data)
  phtest[, "label"] = mtest$df$Fat

  lrn = makeLearner("regr.fregre.glm")
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtest, fd.features = NULL, exclude.cols = "label")
  task = makeRegrTask(data = fdata, target = "label")

  m = train(lrn, task)
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)

  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(cp2, as.numeric(p2), tolerance = .00001)
  expect_equal(cp, as.numeric(p1), tolerance = .00001)
})
