context("tuneDesign")

test_that("tuneDesign", {
  lrn = makeLearner("classif.ksvm")
  reso = 3L
  c.seq = 2^seq(-2, 2, length.out = reso)
  sigma.seq = 2^seq(-2, 2, length.out = reso)
  ps1 = makeParamSet(
    makeDiscreteParam("C", values = c.seq),
    makeDiscreteParam("sigma", values = sigma.seq)
  )
  rin = makeFixedHoldoutInstance(train.inds = seq(1, 150, 2),
    test.inds = seq(2, 150, 2), size = 150)
  # discretized param set
  des = expand.grid(C = c.seq, sigma = sigma.seq)
  des.f = do.call(cbind.data.frame, lapply(des, factor))
  ctrl = makeTuneControlDesign(design = des.f)
  tr1 = tuneParams(lrn, multiclass.task, rin, par.set = ps1, control = ctrl,
    measures = acc)
  expect_number(tr1$y, lower = 0.8, upper = 1)
  op1 = as.data.frame(tr1$opt.path)
  op1$C = as.numeric(as.character(op1$C))
  op1$sigma = as.numeric(as.character(op1$sigma))
  expect_true(all(sortByCol(op1, c("C", "sigma"))[,
    c("C", "sigma")] == sortByCol(des, c("C", "sigma"))))
})

test_that("tuneDesign works with dependent params", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower = 1, upper = 2),
    makeNumericParam("sigma", lower = 1, upper = 2,
      requires = quote(kernel == "rbfdot"))
  )
  lrn = makeLearner("classif.ksvm")
  rdesc = makeResampleDesc("Holdout")
  des = generateGridDesign(par.set = ps, resolution = 3L)
  ctrl = makeTuneControlDesign(design = des)
  tr = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  expect_number(tr$y, lower = 0, upper = 0.2)
  expect_true(getOptPathLength(tr$opt.path) == 3 + 3 * 3)
  expect_true(!is.na(tr$y))
})
