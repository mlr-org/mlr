context("Rlearner_classif_classiFunc.kernel")

test_that("classif_classiFunc.kernel behaves like original api", {
  requirePackagesOrSkip("classiFunc", default.method = "load")

  data(ArrowHead, package = "classiFunc")
  arrow.head = ArrowHead
  classes = arrow.head[, "target"]
  arrow.head = arrow.head[, colnames(arrow.head) != "target"]

  test.inds = sample(1:nrow(arrow.head), size = 0.8 * nrow(arrow.head), replace = FALSE)
  test.inds = (1:nrow(arrow.head))[!(1:nrow(arrow.head)) %in% test.inds]

  mlearn = arrow.head[test.inds, ]
  glearn = classes[test.inds]

  mtest = arrow.head[test.inds, ]
  gtest = classes[test.inds]

  # classiFunc implementation
  a1 = classiFunc::classiKernel(glearn, mlearn, h = 1, nderiv = 1)

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)


  p1.prob = predict(a1, mtest, predict.type = "prob")
  p2.prob = predict(a1, mlearn, predict.type = "prob")

  #------------------------------------------------------------------------------

  # getting the data ready for mlr
  ph = as.data.frame(mlearn)
  ph[, "label"] = glearn
  phtst = as.data.frame(mtest)
  phtst[, "label"] = gtest

  # mlr interface
  lrn = makeLearner("classif.classiFunc.kernel", h = 1, nderiv = 1)

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

  #------------------------------------------------------------------------------
  # test that predict.type = "prob" works
  lrn.prob = makeLearner("classif.classiFunc.kernel",
    h = 1,
    nderiv = 1,
    predict.type = "prob")
  m.prob = train(lrn.prob, task)

  cp.prob = predict(m.prob, newdata = ftest)
  cp2.prob = predict(m.prob, newdata = fdata)
  expect_equal(class(cp.prob)[1],  "PredictionClassif")

  expect_equal(as.matrix(getPredictionProbabilities(cp2.prob)), p2.prob)
  expect_equal(as.matrix(getPredictionProbabilities(cp.prob)), p1.prob)
})

test_that("resampling classiFunc.kernel", {
  requirePackagesOrSkip("classiFunc", default.method = "load")
  lrn = makeLearner("classif.classiFunc.kernel", predict.type = "prob")

  r = resample(lrn, fda.binary.gp.task.small, cv2)
  expect_class(r, "ResampleResult")
})
