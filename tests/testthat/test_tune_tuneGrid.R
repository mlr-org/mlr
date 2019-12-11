context("tuneGrid")

test_that("tuneGrid", {
  lrn = makeLearner("classif.ksvm")
  reso = 3L
  c.seq = 2^seq(-2, 2, length.out = reso)
  sigma.seq = 2^seq(-2, 2, length.out = reso)
  sigma.seq.2 = 2^seq(-2, 2, length.out = reso * 2)
  rin = makeFixedHoldoutInstance(train.inds = seq(1, 150, 2),
    test.inds = seq(2, 150, 2), size = 150)
  # discretized param set
  ps1 = makeParamSet(
    makeDiscreteParam("C", values = c.seq),
    makeDiscreteParam("sigma", values = sigma.seq)
  )
  ctrl = makeTuneControlGrid()
  tr1 = tuneParams(lrn, multiclass.task, rin, par.set = ps1, control = ctrl)
  expect_number(tr1$y, lower = 0, upper = 0.2)
  op1 = as.data.frame(tr1$opt.path)
  op1$C = as.numeric(as.character(op1$C))
  op1$sigma = as.numeric(as.character(op1$sigma))

  ps1.2 = makeParamSet(
    makeDiscreteParam("C", values = c.seq),
    makeDiscreteParam("sigma", values = sigma.seq.2)
  )
  tr1.2 = tuneParams(lrn, multiclass.task, rin, par.set = ps1.2, control = ctrl,
    measures = acc)
  expect_number(tr1.2$y, lower = 0.8, upper = 1)
  op1.2 = as.data.frame(tr1.2$opt.path)
  op1.2$C = as.numeric(as.character(op1.2$C))
  op1.2$sigma = as.numeric(as.character(op1.2$sigma))

  # normal param set
  ps2 = makeParamSet(
    makeNumericParam("C", -2, 2, trafo = function(x) 2^x),
    makeNumericParam("sigma", -2, 2, trafo = function(x) 2^x)
  )
  ctrl = makeTuneControlGrid(resolution = reso)
  tr2 = tuneParams(lrn, multiclass.task, rin, par.set = ps2, control = ctrl)
  op2 = as.data.frame(trafoOptPath(tr2$opt.path))
  op1$exec.time = op2$exec.time = NULL
  expect_equal(sortByCol(op1, c("C", "sigma")), sortByCol(op2, c("C", "sigma")))

  ctrl = makeTuneControlGrid(resolution = c(C = reso, sigma = reso * 2))
  tr2.2 = tuneParams(lrn, multiclass.task, rin, par.set = ps2, control = ctrl,
    measures = acc)
  op2.2 = as.data.frame(trafoOptPath(tr2.2$opt.path))
  op1.2$exec.time = op2.2$exec.time = NULL
  expect_equal(sortByCol(op1.2, c("C", "sigma")),
    sortByCol(op2.2, c("C", "sigma")))
})

test_that("tuneGrid works with dependent params", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower = 1, upper = 2),
    makeNumericParam("sigma", lower = 1, upper = 2,
      requires = quote(kernel == "rbfdot"))
  )
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(resolution = 3L)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  expect_true(getOptPathLength(tr$opt.path) == 3 + 3 * 3)
  expect_true(!is.na(tr$y))
})

test_that("makeTuneControlGrid throws an error, if budget setting is not appropriate", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower = 1, upper = 2),
    makeNumericParam("sigma", lower = 1, upper = 2,
      requires = quote(kernel == "rbfdot"))
  )
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(resolution = 3L, budget = 50L)
  expect_error(tuneParams(lrn, multiclass.task, rdesc, par.set = ps,
    control = ctrl, show.info = FALSE))
})
