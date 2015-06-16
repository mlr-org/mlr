context("tuneGrid")

test_that("tuneGrid", {
  lrn = makeLearner("classif.ksvm")
  reso = 3L
  c.seq = 2^seq(-2, 2, length.out = reso)
  sigma.seq = 2^seq(-2, 2, length.out = reso)
  rin = makeFixedHoldoutInstance(train.inds = seq(1, 150, 2), test.inds = seq(2, 150, 2), size = 150)
  # discretized param set
  ps1 = makeParamSet(
    makeDiscreteParam("C", values = c.seq),
    makeDiscreteParam("sigma", values = sigma.seq)
  )
  ctrl = makeTuneControlGrid()
  tr1 = tuneParams(lrn, multiclass.task, rin, par.set = ps1, control = ctrl)
  op1 = as.data.frame(tr1$opt.path)
  op1$C = as.numeric(as.character(op1$C))
  op1$sigma = as.numeric(as.character(op1$sigma))

  # normal param set
  ps2 = makeParamSet(
    makeNumericParam("C", -2, 2, trafo = function(x) 2^x),
    makeNumericParam("sigma", -2, 2, trafo = function(x) 2^x)
  )
  ctrl = makeTuneControlGrid(resolution = reso)
  tr2 = tuneParams(lrn, multiclass.task, rin, par.set = ps2, control = ctrl)
  op2 = as.data.frame(trafoOptPath(tr2$opt.path))
  op1$exec.time = op2$exec.time = op1$error.message = NULL
  expect_equal(sortByCol(op1, c("C", "sigma")), sortByCol(op2, c("C", "sigma")))

})

test_that("tuneGrid works with dependent params", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower = 1, upper = 2),
    makeNumericParam("sigma", lower = 1, upper = 2, requires = quote(kernel == "rbfdot"))
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
    makeNumericParam("sigma", lower = 1, upper = 2, requires = quote(kernel == "rbfdot"))
  )
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(resolution = 3L, budget = 50L)
  expect_error(tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl, show.info = FALSE))
})
