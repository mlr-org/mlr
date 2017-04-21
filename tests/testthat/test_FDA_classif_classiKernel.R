context("FDA_classif_classiKernel")

test_that("FDA_classif_classiKnn behaves like original api", {
  requirePackagesOrSkip("classiFunc", default.method = "load")

  data(ArrowHead, package = "classiFunc")
  classes = ArrowHead[,"target"]
  ArrowHead = ArrowHead[,colnames(ArrowHead) != "target"]

  set.seed(getOption("mlr.debug.seed"))
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  mlearn = ArrowHead[train_inds,]
  glearn = classes[train_inds]

  mtest = ArrowHead[test_inds,]
  gtest = classes[test_inds]

  # classiFunc implementation
  set.seed(getOption("mlr.debug.seed"))
  a1 = classiFunc::classiKernel(glearn, mlearn, h = 1)

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)


  p1.prob = predict(a1, mtest, predict.type = "prob")
  p2.prob = predict(a1, mlearn, predict.type = "prob")

  ph = as.data.frame(mlearn)
  ph[,"label"] = glearn

  # mlr interface
  lrn = makeLearner("fdaclassif.classiKernel")
  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)
  cp = predict(m, newdata = as.data.frame(mtest))
  cp = unlist(cp$data, use.names = FALSE)

  cp2 = predict(m, newdata = as.data.frame(mlearn))
  cp2 = unlist(cp2$data, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))


  # test that predict.type = "prob" works
  set.seed(getOption("mlr.debug.seed"))
  lrn.prob = makeLearner("fdaclassif.classiKernel",
                         predict.type = "prob")
  m.prob = train(lrn.prob, task)

  cp.prob = predict(m.prob, newdata = as.data.frame(mtest))
  cp2.prob = predict(m.prob, newdata = as.data.frame(mlearn))

  expect_equal(as.matrix(getPredictionProbabilities(cp2.prob)), p2.prob)
  expect_equal(as.matrix(getPredictionProbabilities(cp.prob)), p1.prob)
})
