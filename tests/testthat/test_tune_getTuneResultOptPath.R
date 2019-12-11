context("getTuneResultOptPath")

test_that("getTuneResultOptPath", {
  ctrl = makeTuneControlRandom(maxit = 3L)
  rdesc = makeResampleDesc("CV", iters = 3L)
  ps = makeParamSet(
    makeDiscreteParam("C", values = seq(1:10)
  ))
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = iris.task, resampling = rdesc,
    par.set = ps, control = ctrl)

  expect_equal(res$opt.path, getTuneResultOptPath(res, as.df = FALSE))
  expect_equal(as.data.frame(res$opt.path), getTuneResultOptPath(res))
})
