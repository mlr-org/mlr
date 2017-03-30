context("FDA_classif_classiKnn")

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
  a1 = classiFunc::classiKnn(glearn, mlearn, knn = 1L)

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)


  p1.prob = predict(a1, mtest, predict.type = "prob")
  p2.prob = predict(a1, mlearn, predict.type = "prob")

  ph = as.data.frame(mlearn)
  ph[,"label"] = glearn

  # mlr interface
  lrn = makeLearner("fdaclassif.classiKnn")
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
  lrn.prob = makeLearner("fdaclassif.classiKnn",
                         predict.type = "prob")
  m.prob = train(lrn.prob, task)

  cp.prob = predict(m.prob, newdata = as.data.frame(mtest))
  cp2.prob = predict(m.prob, newdata = as.data.frame(mlearn))

  expect_equal(as.matrix(getPredictionProbabilities(cp2.prob)), p2.prob)
  expect_equal(as.matrix(getPredictionProbabilities(cp.prob)), p1.prob)
})

test_that("FDA_classif_classiKnn behaves like original api for different metrics", {
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

  set.seed(getOption("mlr.debug.seed"))
  a.metric = classiFunc::classiKnn(glearn, mlearn,
                                   metric = "Minkowski", p = 1)
  p1.metric = predict(a.metric, mtest)
  p2.metric = predict(a.metric, mlearn)


  ph = as.data.frame(mlearn)
  ph[,"label"] = glearn

  lrn.metric = makeLearner(cl = "fdaclassif.classiKnn",
                           par.vals = list(metric = "Minkowski",
                                           p = 1))

  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m.metric = train(lrn.metric, task)
  cp.metric = predict(m.metric, newdata = as.data.frame(mtest))
  cp.metric = unlist(cp.metric$data, use.names = FALSE)

  cp2.metric = predict(m.metric, newdata = as.data.frame(mlearn))
  cp2.metric = unlist(cp2.metric$data, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2.metric), as.character(p2.metric))
  expect_equal(as.character(cp.metric), as.character(p1.metric))


  # test that some other metrics work basically
  metrics = c("globMax", "jump", "max")
  short.task = subsetTask(task, subset = c(TRUE, FALSE, FALSE))
  lrn.metrics = list()
  for(i in 1:length(metrics)) {
    lrn.metrics[[i]] = makeLearner(cl = "fdaclassif.classiKnn",
                                   par.vals = list(metric = metrics[i]))
  }

  m.metrics = list()
  for(i in 1:length(metrics)) {
    m.metrics[[i]] = train(lrn.metrics[[i]], short.task)
  }

})


test_that("FDA_classif_classiKnn handles upper and lower bounds correctly", {
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

  ph = as.data.frame(mlearn)
  ph[,"label"] = glearn

  lrn.shortEucl = makeLearner(cl = "fdaclassif.classiKnn",
                           par.vals = list(dmax = 10,
                                           metric = "shortEuclidean"
                                           ))

  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m.shortEucl = train(lrn.shortEucl, task)
  p.shortEucl = predict(m.shortEucl, newdata = as.data.frame(mtest))

  # check that all expressions are evaluated
  par.set = getParamSet(m.shortEucl$learner)
  expect_equal(par.set, evaluateParamExpressions(par.set))

  # check that tuning works
  tune.par.set = par.set
  tune.par.set$pars = par.set$pars[c("dmin", "dmax")]
  # Take out constraint on knn, because we do not tune it
  tune.par.set$forbidden = par.set$forbidden[1]

  tuned.params = tuneParams(learner = m.shortEucl$learner,
             resampling = makeResampleDesc("CV", iters = 2L),
             task = task,
             par.set = tune.par.set,
             control = makeTuneControlRandom(maxit = 10L))
  expect_class(tuned.params, "TuneResult")

})

# # TODO test that passing on stuff to fda::Data2fd() works
# test_that("FDA_classif_classiKnn behaves like original api for additional arguments to to fda::Data2fd()", {
#   requirePackagesOrSkip("classiFunc", default.method = "load")
#   stop("TODO")
# })

