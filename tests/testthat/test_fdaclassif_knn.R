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

  # fda.usc implementation
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.knn(glearn, mlearn, knn = 1L, par.CV = list(trim = 0.5))

  p1 = predict(a1, mtest)
  p2 = predict(a1, mlearn)

  p1.prob = predict(a1, mtest, type = "probs")$prob.group
  p2.prob = predict(a1, mlearn, type = "probs")$prob.group

  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn

  # mlr interface
  lrn = makeLearner("fdaclassif.knn",
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


  # test that predict.type = "prob" works
  set.seed(getOption("mlr.debug.seed"))
  lrn.prob = makeLearner("fdaclassif.knn",
                         par.vals = list(knn = 1L, trim = 0.5),
                         predict.type = "prob")
  m.prob = train(lrn.prob, task)

  cp.prob = predict(m.prob, newdata = as.data.frame(mtest$data))
  cp2.prob = predict(m.prob, newdata = as.data.frame(mlearn$data))

  expect_equal(as.matrix(getPredictionProbabilities(cp2.prob)), p2.prob)
  expect_equal(as.matrix(getPredictionProbabilities(cp.prob)), p1.prob)


  # test that parameters work for different metrics
  set.seed(getOption("mlr.debug.seed"))
  a.metric = fda.usc::classif.knn(glearn, mlearn,
                                  metric = fda.usc::semimetric.deriv,
                                  par.CV = list(trim = 0.5))
  p1.metric = predict(a.metric, mtest)
  p2.metric = predict(a.metric, mlearn)


  lrn.metric = makeLearner(cl = "fdaclassif.knn",
                           par.vals = list(trim = 0.5, knn = 1L,
                                           metric = "semimetric.deriv"))

  set.seed(getOption("mlr.debug.seed"))
  m.metric = train(lrn.metric, task)
  cp.metric = predict(m.metric, newdata = as.data.frame(mtest$data))
  cp.metric = unlist(cp.metric$data, use.names = FALSE)

  cp2.metric = predict(m.metric, newdata = as.data.frame(mlearn$data))
  cp2.metric = unlist(cp2.metric$data, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2.metric), as.character(p2.metric))
  expect_equal(as.character(cp.metric), as.character(p1.metric))



  short.task = subsetTask(task, subset = c(TRUE, FALSE, FALSE))
  # test that all metrics work basically
  metrics = c("metric.lp",
              "metric.hausdorff",
              "inprod.fdata",
              "semimetric.basis",
              "semimetric.deriv",
              "semimetric.fourier",
              "semimetric.hshift",
              # "semimetric.mplsr",
              "semimetric.pca")
  lrn.metrics = list()
  for(i in 1:length(metrics)) {
    lrn.metrics[[i]] = makeLearner(cl = "fdaclassif.knn",
                                   par.vals = list(knn = 1L, trim = 0.5,
                                                   metric = metrics[i]))
  }

  m.metrics = list()
  for(i in 1:length(metrics)) {
    m.metrics[[i]] = train(lrn.metrics[[i]], short.task)
  }

})

test_that("FDA_classif_knn can hand down arguments to create...basis if
type = 'semimetric.basis'", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  # Use only 10 obs. for 2 classes, as knn training is really slow
  index = c(1:10, 50:60)
  mlearn$data = mlearn$data[index,]
  glearn = phoneme[["classlearn"]][index]
  glearn = droplevels(glearn)

  # fda.usc implementation
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.knn(glearn, mlearn, knn = 1L, par.CV = list(trim = 0.5),
                            metric = fda.usc::semimetric.basis,
                            type.basis1 = "bspline", nbasis1 = 10)

  ph = as.data.frame(mlearn$data)
  ph[,"label"] = glearn

  # mlr interface
  lrn = makeLearner("fdaclassif.knn",
                    par.vals = list(knn = 1L, trim = 0.5, nbasis1 = 10,
                                    metric = "semimetric.basis",
                                    type.basis1 = "bspline"))
  task = makeFDAClassifTask(data = ph, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)

  expect_equal(a1$mdist, m$learner.model$mdist)
  })

