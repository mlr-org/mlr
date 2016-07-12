context("hyperparameterValidation")

test_that("1 numeric hyperparam", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("C", values = 2^(-5:5)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 3L)
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
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  
  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]), 
                   c("GeomPoint", "GeomLine"))
  expect_equal(plt$labels$x, "iteration")
  expect_equal(plt$labels$y, "Accuracy")
  
  # test for global
  orig$acc.test.mean = cummax(orig$acc.test.mean)
  expect_equivalent(plt$data, orig) 
  
  # FIXME: make sure plot looks as expected
})

test_that("1 discrete hyperparam", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("kernel", values = c("vanilladot", 
                                                           "polydot", "rbfdot"))
                    )
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc, 
                   par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)
  
  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "kernel", y = "acc.test.mean")
  print(plt)
  dir = tempdir()
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  
  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]), 
                   "GeomPoint")
  expect_equal(plt$labels$x, "kernel")
  expect_equal(plt$labels$y, "Accuracy")
  
  # FIXME: make sure plot looks as expected
})

test_that("1 numeric hyperparam with optimizer failure", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("C", values = c(-1, 0.5, 1.5, 1, 0.2, 0.3, 
                                                      0.4))
  )
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 3L)
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
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  
  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]), 
                   "GeomPoint")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "Accuracy")
  
  # FIXME: make sure plot looks as expected
})

test_that("1 numeric hyperparam with nested cv", {
  # generate data
  ps = makeParamSet(makeNumericParam("C", lower = 0.01, upper = 2)
  )
  ctrl = makeTuneControlRandom(maxit = 25L)
  rdesc = makeResampleDesc("CV", iters = 3L)
  lrn = makeTuneWrapper("classif.ksvm", control = ctrl, 
                        resampling = rdesc, par.set = ps, 
                        show.info = F)
  res = resample(lrn, task = pid.task, resampling = cv2, 
                 extract = getTuneResult)
  orig = getNestedTuneResultsOptPathDf(res)
  new = generateHyperParsEffectData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)
  
  # make sure plot is created and can be saved
  plt = plotHyperParsEffect(new, x = "C", y = "mmce.test.mean", 
                            facet = "nested_cv_run")
  print(plt)
  dir = tempdir()
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  
  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]), 
                   "GeomPoint")
  expect_equal(plt$labels$x, "C")
  expect_equal(plt$labels$y, "Mean misclassification error")
  
  # FIXME: make sure plot looks as expected
})
