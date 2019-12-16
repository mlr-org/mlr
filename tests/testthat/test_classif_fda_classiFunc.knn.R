context("Rlearner_classif_classiFunc.knn")

test_that("classif_classiFunc.knn behaves like original api", {
  requirePackagesOrSkip("classiFunc", default.method = "load")

  data(ArrowHead, package = "classiFunc")
  arrow.head = ArrowHead
  classes = arrow.head[, "target"]
  arrow.head = arrow.head[, colnames(arrow.head) != "target"]

  train.inds = sample(1:nrow(arrow.head), size = 0.8 * nrow(arrow.head), replace = FALSE)
  test.inds = (1:nrow(arrow.head))[!(1:nrow(arrow.head)) %in% train.inds]

  mlearn = arrow.head[train.inds, ]
  glearn = classes[train.inds]

  mtest = arrow.head[test.inds, ]
  gtest = classes[test.inds]

  # classiFunc implementation
  a1 = classiFunc::classiKnn(glearn, mlearn, knn = 1L)

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)


  p1.prob = predict(a1, mtest, predict.type = "prob")
  p2.prob = predict(a1, mlearn, predict.type = "prob")

  # getting the data ready for mlr
  ph = as.data.frame(mlearn)
  ph[, "label"] = glearn
  phtst = as.data.frame(mtest)
  phtst[, "label"] = gtest

  # mlr interface
  lrn = makeLearner("classif.classiFunc.knn")

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


  # test that predict.type = "prob" works
  lrn.prob = makeLearner("classif.classiFunc.knn",
    predict.type = "prob")
  m.prob = train(lrn.prob, task)

  cp.prob = predict(m.prob, newdata = ftest)
  cp2.prob = predict(m.prob, newdata = fdata)
  expect_equal(class(cp.prob)[1],  "PredictionClassif")

  expect_equal(as.matrix(getPredictionProbabilities(cp2.prob)), p2.prob)
  expect_equal(as.matrix(getPredictionProbabilities(cp.prob)), p1.prob)
})

test_that("resampling classiFunc.knn", {
  requirePackagesOrSkip("classiFunc", default.method = "load")
  lrn = makeLearner("classif.classiFunc.knn", predict.type = "prob")

  r = resample(lrn, fda.binary.gp.task.small, cv2)
  expect_class(r, "ResampleResult")
})

test_that("classiFunc.knn can be predicted in parallel", {
  requirePackagesOrSkip(c("classiFunc", "parallelMap"), default.method = "load")

  data(ArrowHead, package = "classiFunc")
  arrow.head = ArrowHead

  lrn = makeLearner("classif.classiFunc.knn")

  # create task
  fdata = makeFunctionalData(arrow.head, exclude.cols = "target")
  task = makeClassifTask(data = fdata, target = "target")

  # train model
  m = train(lrn, task)
  cp = predict(m, task = task)

  # Parallelize across 2 CPUs
  # set up parallelization
  parallelMap::parallelStartSocket(cpus = 2L) # parallelStartMulticore(cpus = 2L) for Linux

  # predict in parallel
  # specify parallel = TRUE and batchsize > 1L for parallelization
  cp.parallel = predict(m, task = task, parallel = TRUE, batches = 2L)

  # clean up parallelization
  parallelMap::parallelStop()

  # results do not change
  expect_equal(cp$data, cp.parallel$data)
})

test_that("rucrdtw can be used as distance measure in classiFunc.knn", {
  requirePackagesOrSkip(c("classiFunc", "rucrdtw"), default.method = "load")

  data(ArrowHead, package = "classiFunc")
  arrow.head = ArrowHead

  lrn = makeLearner("classif.classiFunc.knn",
    par.vals = list(metric = "L2", knn = 3),
    predict.type = "prob")

  # create task
  fdata = makeFunctionalData(arrow.head, exclude.cols = "target")
  task = makeClassifTask(data = fdata, target = "target")

  # train model
  m = train(lrn, task)
  cp = predict(m, task = task)
  expect_class(cp, "PredictionClassif")
  expect_data_frame(cp$data)
})
