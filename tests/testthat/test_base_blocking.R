context("blocking")

test_that("blocking with single resampling", {
  df = multiclass.df
  b = as.factor(rep(1:30, 5))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df, blocking = b)
  expect_true(getTaskDesc(ct)$has.blocking)
  res = makeResampleInstance(makeResampleDesc("Blocking"), task = ct)
  for (j in 1:res$desc$iters) {
    train.j = res$train.inds[[j]]
    test.j = res$test.inds[[j]]
    tab = table(b[train.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    tab = table(b[test.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
  }
  # test blocking in resample
  lrn = makeLearner("classif.lda")
  mycheck = function(rdesc, p, b) {
    for (j in 1:rdesc$iters) {
      test.j = p$data[p$data$iter == j, "id"]
      tab = table(b[test.j])
      expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    }
  }

  rdesc = makeResampleDesc("Blocking")

  # single resampling working!
  p = resample(lrn, ct, rdesc)$pred
  mycheck(rdesc, p, b)


  rdesc = makeResampleDesc("RepCV", folds = 3, reps = 2)
  p = resample(lrn, ct, rdesc)$pred
  mycheck(rdesc, p, b)
})

test_that("blocking with nested resampling", {
  df = multiclass.df
  b = as.factor(rep(1:5, rep(30, 5)))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df, blocking = b)
  expect_true(getTaskDesc(ct)$has.blocking)
  res = makeResampleInstance(makeResampleDesc("Blocking"), task = ct)
  for (j in 1:res$desc$iters) {
    train.j = res$train.inds[[j]]
    test.j = res$test.inds[[j]]
    tab = table(b[train.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    tab = table(b[test.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
  }
  # test blocking in resample
  lrn = makeLearner("classif.lda")
  mycheck = function(rdesc, p, b) {
    for (j in 1:rdesc$iters) {
      test.j = p$data[p$data$iter == j, "id"]
      tab = table(b[test.j])
      expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    }
  }

  ctrl <- makeTuneControlRandom(maxit = 200)
  ps <- makeParamSet(makeNumericParam("nu", lower = 2, upper = 20))
  inner = makeResampleDesc("CV", iters = 3)
  # outer = makeResampleDesc("CV", iters = 5)
  outer = makeResampleDesc("Blocking")
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl,
                                 show.info = FALSE)

  p = resample(tune_wrapper, ct, outer, show.info = TRUE, extract = getTuneResult)

  p = resample(tune_wrapper, ct, outer)$pred
  mycheck(rdesc, p, b)


  ## Double blocking

  inner = makeResampleDesc("Blocking")
  # outer = makeResampleDesc("CV", iters = 5)
  outer = makeResampleDesc("Blocking")
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl,
                                 show.info = FALSE)

  p = resample(tune_wrapper, ct, outer, show.info = TRUE, extract = getTuneResult)


  # SPATIAL

  rownames(spatial.task$env$data) = c(1:750)
  spatial.task$env$data = spatial.task$env$data[1:750, ]
  spatial.task$coordinates = spatial.task$coordinates[1:750, ]
  spatial.df = spatial.task$env$data[1:750, ]

  # single

  lrn = makeLearner("classif.lda")
  rdesc = makeResampleDesc("Blocking")
  ct = makeClassifTask(target = "slides", data = spatial.df, blocking = b, coordinates = spatial.task$coordinates)
  p = resample(lrn, ct, rdesc)

  # nested

  #rownames(spatial.df) = c(1:750)
  b = as.factor(rep(1:5, rep(150, 5)))
  ct = makeClassifTask(target = "slides", data = spatial.df, blocking = b, coordinates = spatial.task$coordinates)

  lrn = makeLearner("classif.lda")
  ctrl <- makeTuneControlRandom(maxit = 200)
  ps <- makeParamSet(makeNumericParam("nu", lower = 2, upper = 20))
  inner = makeResampleDesc("Blocking")
  #outer = makeResampleDesc("CV", iters = 5)
  outer = makeResampleDesc("Blocking")
  #outer = makeResampleDesc("SpCV", iters = 5)
  tune_wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl,
                                 show.info = FALSE)

  p = resample(tune_wrapper, ct, outer, show.info = TRUE, extract = getTuneResult)


  plot = createSpatialResamplingPlots(spatial.task, list("Blocking" = p), crs = 32717)
  plot_grid(plotlist = plot[["Plots"]], ncol = 5, nrow = 1, labels = plot[["Labels"]])

})
