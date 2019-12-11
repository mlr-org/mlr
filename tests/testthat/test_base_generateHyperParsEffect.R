context("hyperparameterValidation")

test_that("generate data", {
  # generate data with nested no trafo
  ps = makeParamSet(makeNumericParam("C", lower = -5, upper = 5,
    trafo = function(x) 2^x)
  )
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout")
  lrn = makeTuneWrapper("classif.ksvm", control = ctrl,
    resampling = rdesc, par.set = ps,
    show.info = FALSE, measures = getDefaultMeasure(pid.task))
  res = resample(lrn, task = pid.task, resampling = cv2,
    extract = getTuneResult)
  orig = getNestedTuneResultsOptPathDf(res)
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)

  # generate data, no include diag, trafo
  rdesc = makeResampleDesc("Holdout")
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
    par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(trafoOptPath(res$opt.path))
  orig = dropNamed(orig, c("eol", "error.message"))
  names(orig)[names(orig) == "dob"] = "iteration"
  new = generateHyperParsEffectData(res, trafo = TRUE)
  expect_equivalent(new$data, orig)
})

test_that("1 numeric hyperparam", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("C", values = 2^(-2:2)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("Holdout")
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
    par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  orig$C = as.numeric(as.character(orig$C))
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)

  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "iteration", y = "acc.test.mean",
    plot.type = "line")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)

  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomPoint", "GeomLine"))
  expect_equal(plt$labels$x, "iteration")
  expect_equal(plt$labels$y, "acc.test.mean")

  # test facet 1D with non-nested shouldn't produce
  expect_error(plotHyperParsEffect(new, x = "iteration", y = "acc.test.mean",
    plot.type = "line", facet = "nested_cv_run")
  )
})

test_that("1 discrete hyperparam", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("kernel", values = c("vanilladot",
    "polydot", "rbfdot"), default = "rbfdot")
  )
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("Holdout")
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
    par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)

  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "kernel", y = "acc.test.mean")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)

  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    "GeomPoint")
  expect_equal(plt$labels$x, "kernel")
  expect_equal(plt$labels$y, "acc.test.mean")
})

test_that("1 numeric hyperparam with optimizer failure", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("C", values = c(-1, 0.5, 1.5)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("Holdout")
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
    par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  orig$C = as.numeric(as.character(orig$C))
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)

  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "C", y = "acc.test.mean")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)

  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    "GeomPoint")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "acc.test.mean")
})

test_that("1 numeric hyperparam with nested cv", {
  # generate data
  ps = makeParamSet(makeNumericParam("C", lower = 0.01, upper = 2))
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout")
  lrn = makeTuneWrapper("classif.ksvm", control = ctrl,
    resampling = rdesc, par.set = ps, show.info = FALSE,
    measures = getDefaultMeasure(pid.task))
  res = resample(lrn, task = pid.task, resampling = cv2,
    extract = getTuneResult)
  orig = getNestedTuneResultsOptPathDf(res)
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)

  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "C", y = "mmce.test.mean")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)

  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    "GeomPoint")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "mmce.test.mean")
})

test_that("2 hyperparams", {
  # generate data
  ps = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x))
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "rbfdot"))
  res = tuneParams(learn, task = pid.task, control = ctrl, measures = acc,
    resampling = rdesc, par.set = ps, show.info = FALSE)
  data = generateHyperParsEffectData(res)

  # test line creation
  plt = plotHyperParsEffect(data, x = "iteration", y = "acc.test.mean",
    plot.type = "line")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomLine", "GeomPoint"))
  expect_equal(plt$labels$x, "iteration")
  expect_equal(plt$labels$y, "acc.test.mean")

  # test heatcontour creation with interpolation
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "heatmap", interpolate = "regr.earth",
    show.experiments = TRUE)
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomPoint", "GeomRaster"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")
  expect_equal(plt$labels$shape, "learner_status")

  # learner crash
  ps = makeParamSet(
    makeDiscreteParam("C", values = c(-1, 0.5, 1.5)),
    makeDiscreteParam("sigma", values = c(-1, 0.5, 1.5)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("Holdout")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "rbfdot"))
  res = tuneParams(learn, task = pid.task, control = ctrl, measures = acc,
    resampling = rdesc, par.set = ps, show.info = FALSE)
  data = generateHyperParsEffectData(res)
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "heatmap", interpolate = "regr.earth")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomPoint", "GeomRaster"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")
  expect_equal(plt$labels$shape, "learner_status")
})

test_that("2 hyperparams nested", {
  # generate data
  ps = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x))
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "rbfdot"))
  lrn = makeTuneWrapper(learn, control = ctrl,
    measures = list(acc, mmce), resampling = rdesc,
    par.set = ps, show.info = FALSE)
  res = resample(lrn, task = pid.task, resampling = cv2,
    extract = getTuneResult)
  data = generateHyperParsEffectData(res)

  # contour plot
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "contour", interpolate = "regr.earth",
    show.interpolated = TRUE)
  expect_warning(print(plt))
  dir = tempdir()
  path = file.path(dir, "test.svg")
  expect_warning(ggsave(path))
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomPoint", "GeomRaster", "GeomContour"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")
  expect_equal(plt$labels$shape, "learner_status")

  # test with nested facetting
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "heatmap", interpolate = "regr.earth",
    show.experiments = TRUE, facet = "nested_cv_run")
  print(plt)
  dir = tempdir()
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  expect_set_equal(class(plt$facet)[1], "FacetWrap")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")
  expect_equal(plt$labels$shape, "learner_status")

  # learner crashes
  ps = makeParamSet(
    makeDiscreteParam("C", values = c(-1, 0.5, 1.5)),
    makeDiscreteParam("sigma", values = c(-1, 0.5, 1.5)))
  lrn = makeTuneWrapper(learn, control = ctrl,
    measures = list(acc, mmce), resampling = rdesc,
    par.set = ps, show.info = FALSE)
  res = resample(lrn, task = pid.task, resampling = cv2,
    extract = getTuneResult)
  data = generateHyperParsEffectData(res)
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "heatmap", interpolate = "regr.earth",
    show.experiments = TRUE)
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomPoint", "GeomRaster"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")
  expect_equal(plt$labels$shape, "learner_status")
})

test_that("2+ hyperparams", {
  # generate data
  ps = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeDiscreteParam("degree", values = 2:5))
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout", predict = "both")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "besseldot"))
  res = tuneParams(learn, task = pid.task, control = ctrl,
    measures = list(acc, setAggregation(acc, train.mean)), resampling = rdesc,
    par.set = ps, show.info = FALSE)
  data = generateHyperParsEffectData(res, partial.dep = TRUE)

  # test single hyperparam creation
  plt = plotHyperParsEffect(data, x = "C", y = "acc.test.mean",
    plot.type = "line", partial.dep.learn = "regr.rpart")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomLine", "GeomPoint"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "acc.test.mean")

  # test bivariate
  plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "acc.test.mean",
    plot.type = "heatmap", partial.dep.learn = "regr.rpart")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_equal(class(plt$layers[[1]]$geom)[1], "GeomRaster")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "sigma")
  expect_equal(plt$labels$fill, "acc.test.mean")

  # simple example with nested cv
  ps = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x),
    makeDiscreteParam("degree", values = 2:5))
  ctrl = makeTuneControlRandom(maxit = 5L)
  rdesc = makeResampleDesc("Holdout", predict = "both")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "besseldot"))
  lrn = makeTuneWrapper(learn, control = ctrl,
    measures = list(acc, mmce), resampling = rdesc, par.set = ps,
    show.info = FALSE)
  res = resample(lrn, task = pid.task, resampling = cv2, extract = getTuneResult)
  data = generateHyperParsEffectData(res, partial.dep = TRUE)
  plt = plotHyperParsEffect(data, x = "C", y = "acc.test.mean",
    plot.type = "line", partial.dep.learn = "regr.rpart")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomLine", "GeomPoint"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "acc.test.mean")

  # learner crash with imputation works
  ps = makeParamSet(
    makeDiscreteParam("C", values = c(-1, 0.5, 1.5)),
    makeDiscreteParam("sigma", values = c(-1, 0.5, 1.5)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("Holdout")
  learn = makeLearner("classif.ksvm", par.vals = list(kernel = "besseldot"))
  res = tuneParams(learn, task = pid.task, control = ctrl, measures = acc,
    resampling = rdesc, par.set = ps, show.info = FALSE)
  data = generateHyperParsEffectData(res, partial.dep = TRUE)
  plt = plotHyperParsEffect(data, x = "C", y = "acc.test.mean",
    plot.type = "line", partial.dep.learn = "regr.rpart")
  print(plt)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]),
    c("GeomLine", "GeomPoint"))
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "acc.test.mean")
})
