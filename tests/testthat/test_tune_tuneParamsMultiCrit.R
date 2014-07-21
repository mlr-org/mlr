context("tuneParamsMultiCrit")

test_that("tuneParamsMultiCrit", {
  lrn =  makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeIntegerParam("minsplit", lower=1, upper = 50)
  )
  ctrl = makeTuneMultiCritControlRandom(maxit = 2)
  expect_error(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps, measures = mmce, control = ctrl))

  mycheck = function(res, k) {
    expect_output(print(res), "Points on front")
    expect_true(is.integer(res$ind))
    expect_true(is.list(res$x))
    expect_true(is.matrix(res$y))
    expect_equal(getOptPathLength(res$opt.path), k)
  }

  # random search
  ctrl = makeTuneMultiCritControlRandom(maxit = 2)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 2)
  # and check plotting
  plotTuneMultiCritResult(res, path = TRUE)
  plotTuneMultiCritResult(res, path = FALSE)

  # grid search
  ctrl = makeTuneMultiCritControlGrid(resolution = 2L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 2)

  # nsga2
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 12L)
})


test_that("tuneParamsMultiCrit works with low number of evals and dependencies", {
  # we had a bug here triggered thru code in PH
  ps = makeParamSet(
    makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
    makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x,
      requires = quote(kernel == "rbfdot")),
    makeIntegerParam("degree", lower = 2L, upper = 5L,
      requires = quote(kernel == "polydot"))
  )
  ctrl = makeTuneMultiCritControlRandom(maxit = 1L)
  rdesc = makeResampleDesc("Holdout")
  res = tuneParamsMultiCrit("classif.ksvm", sonar.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
})


