context("getTuneResultOptPath")

test_that("getTuneResultOptPath", {
  
  ctrl = makeTuneControlRandom(maxit = 10L) 
  rdesc = makeResampleDesc("CV", iters = 3L) 
  discrete_ps = makeParamSet( 
    makeDiscreteParam("C", values = seq(1:1000) 
    )) 
  rdesc = makeResampleDesc("CV", iters = 3L) 
  res = tuneParams("classif.ksvm", task = iris.task, resampling = rdesc, 
                   par.set = discrete_ps, control = ctrl) 

  expect_equal(res$opt.path, getTuneResultOptPath(res, as.df = FALSE))
  expect_equal(as.data.frame(res$opt.path), getTuneResultOptPath(res))

})
