context("tuneParamsMultiCrit")

test_that("tuneParamsMultiCrit", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeIntegerParam("minsplit", lower = 1, upper = 50)
  )
  ctrl = makeTuneMultiCritControlRandom(maxit = 2)
  expect_error(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc,
    par.set = ps, measures = mmce, control = ctrl),
  ".* May only contain the following types: Measure.")

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
  print(plotTuneMultiCritResult(res, path = TRUE))
  print(plotTuneMultiCritResult(res, path = FALSE))

  # grid search
  ctrl = makeTuneMultiCritControlGrid(resolution = 2L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 2)

  # nsga2
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 8L)

  # MBO
  ctrl = makeTuneMultiCritControlMBO(2L, budget = 4L * length(ps$pars) + 1L)
  # suppressed warnings: "generateDesign could only produce 50 points instead of
  # 1000!"
  res = suppressWarnings(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  )
  mycheck(res, 4L * length(ps$pars) + 1L)

  # MBO with mbo.control
  # Size of init design is 4 * length(ps$pars) by default of mlrMBO
  mbo.control = mlrMBO::makeMBOControl(n.objectives = 2L)
  mbo.control = mlrMBO::setMBOControlInfill(mbo.control,
    crit = mlrMBO::makeMBOInfillCritDIB())
  mbo.control = mlrMBO::setMBOControlMultiObj(mbo.control)
  mbo.control = mlrMBO::setMBOControlTermination(mbo.control, iters = 1)
  ctrl = makeTuneMultiCritControlMBO(mbo.control = mbo.control)
  # suppressed warnings: "generateDesign could only produce 50 points instead of
  # 1000!"
  res = suppressWarnings(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc,
    par.set = ps, measures = list(tpr, fpr), control = ctrl))
  mycheck(res, 4L * length(ps$pars) + 1L)

  # MBO with dependent param set
  lrn = makeLearner("classif.ksvm")
  ps = makeParamSet(makeDiscreteParam("kernel", c("polydot", "rbfdot")),
    makeNumericParam("sigma", lower = -12, upper = 12,
      trafo = function(x) 2^x, requires = quote(kernel == "rbfdot")))
  ctrl = makeTuneMultiCritControlMBO(2L, budget = 4L * length(ps$pars) + 1L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  mycheck(res, 4L * length(ps$pars) + 1L)
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
  expect_silent(tuneParamsMultiCrit("classif.ksvm", sonar.task, rdesc,
    par.set = ps, measures = list(tpr, fpr), control = ctrl))
})

# FIXME: I am not sure how we can check wich value is imputed for the optimizer?
test_that("y imputing works", {
  configureMlr(on.learner.error = "quiet")
  lrn = makeLearner("classif.__mlrmocklearners__2")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1)
  )
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl)
  ctrl = makeTuneMultiCritControlNSGA2(impute.val = c(100, 100), popsize = 4L,
    generations = 1L)
  expect_silent(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl))

  configureMlr(on.learner.error = "stop")
})

test_that("tuneParamsMultiCrit with budget", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 50)
  )

  mycheck = function(ctrl, expected.budget) {
    if ("TuneMultiCritControlGrid" %in% class(ctrl)) {
      if (!is.null(ctrl$budget)) {
        expect_equal(ctrl$budget, expected.budget)
      }
    } else {
      expect_equal(ctrl$budget, expected.budget)
    }
    res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
      measures = list(tpr, fpr), control = ctrl)
    expect_equal(getOptPathLength(res$opt.path), expected.budget)
  }

  # random search
  ctrl = makeTuneMultiCritControlRandom(maxit = 3L)
  mycheck(ctrl, ctrl$extra.args$maxit)
  ctrl = makeTuneMultiCritControlRandom(maxit = 3L, budget = 3L)
  mycheck(ctrl, ctrl$extra.args$maxit)
  expect_error(makeTuneMultiCritControlRandom(maxit = 3L, budget = 5L),
    "The parameters .* differ.")

  # grid search
  ctrl = makeTuneMultiCritControlGrid(resolution = 3)
  mycheck(ctrl, ctrl$extra.args$resolution^2)
  ctrl = makeTuneMultiCritControlGrid(resolution = 3, budget = 9L)
  mycheck(ctrl, ctrl$extra.args$resolution^2)
  ctrl = makeTuneMultiCritControlGrid(resolution = 3, budget = 10L)
  expect_error(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl),
  ".* does not fit to the size of the grid .*")

  # nsga2
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
  mycheck(ctrl, ctrl$extra.args$popsize * (ctrl$extra.args$generations + 1))
  expect_error(makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 2L,
    budget = 8L),
  ".* contradicts the product of .*")
  expect_error(makeTuneMultiCritControlNSGA2(generations = 4L, budget = 12L),
    ".* contradicts the product of .*")
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, budget = 12L)
  expect_equal(ctrl$extra.args$generations, 2L)
  mycheck(ctrl, 12L)
})

test_that("plotTuneMultiCritResult works with pretty.names", {
  lrn = makeLearner("classif.rpart")
  ps = makeParamSet(
    makeDiscreteParam("minsplit", values = c(5, 10))
  )
  ctrl.grid = makeTuneMultiCritControlGrid()
  opt.multi.crit = tuneParamsMultiCrit(lrn, multiclass.task, hout,
    list(mmce, acc), par.set = ps, control = ctrl.grid)
  expect_silent(plotTuneMultiCritResult(opt.multi.crit))
  expect_silent(plotTuneMultiCritResult(opt.multi.crit, pretty.names = FALSE))
})

test_that("tuneParamsMultiCrit with resample.fun", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeIntegerParam("minsplit", lower = 1, upper = 50)
  )

  # random search
  ctrl = makeTuneMultiCritControlRandom(maxit = 2)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(res$opt.path) == 0.5))

  # grid search
  ctrl = makeTuneMultiCritControlGrid(resolution = 2L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(res$opt.path) == 0.5))

  # nsga2
  ctrl = makeTuneMultiCritControlNSGA2(popsize = 4L, generations = 1L)
  res = tuneParamsMultiCrit(lrn, binaryclass.task, rdesc, par.set = ps,
    measures = list(tpr, fpr), control = ctrl, resample.fun = constant05Resample)
  expect_true(all(getOptPathY(res$opt.path) == 0.5))

  # MBO
  ctrl = makeTuneMultiCritControlMBO(2L, budget = 4L * length(ps$pars) + 1L,
    learner = "regr.lm")
  # suppressed warnings: "generateDesign could only produce 50 points instead of
  # 1000!"
  res = suppressWarnings(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc,
    par.set = ps, measures = list(tpr, fpr), control = ctrl,
    resample.fun = constant05Resample))
  expect_true(all(getOptPathY(res$opt.path) == 0.5))
})

test_that("check n.objectives for MBO multi crit", {
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeIntegerParam("minsplit", lower = 1, upper = 50)
  )

  expect_error(makeTuneMultiCritControlMBO(1L),
    ".* >= 2")
  expect_error(makeTuneMultiCritControlMBO(1.5),
    ".* Must be of type 'single integerish value', not 'double'.")
  ctrl = makeTuneMultiCritControlMBO(2L)

  expect_error(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc,
    measures = list(mmce),
    par.set = ps, control = ctrl),
  ".* Must have length >= 2, but has length 1.")
  expect_error(tuneParamsMultiCrit(lrn, binaryclass.task, rdesc,
    measures = list(mmce, tpr, fpr),
    par.set = ps, control = ctrl),
  ".* Must have length 2, but has length 3.")
})
