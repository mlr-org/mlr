context("tuneOptim")

test_that("tuneOptim", {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParamSet(
    makeNumericParam("C", trafo=function(x) 2^x), 
    makeNumericParam("sigma", trafo=function(x) 2^x)
  )
  ps2 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1)
  )
  ps3 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeDiscreteParam("minsplit", values=c(1,2))
  )
  lrn1 = makeLearner("classif.ksvm")
  lrn2 = makeLearner("classif.rpart")
  # nelder mead with optim
  ctrl = makeTuneControlOptim(method="Nelder-Mead", 
    start=list(C=0, sigma=0), maxit=10)
  tr = tuneParams(lrn1, binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="Nelder-Mead", 
    start=list(cp=0.05, minsplit=5), maxit=10)
  expect_error(tuneParams(lrn2, binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneControlOptim(method="SANN", 
    start=list(C=0, sigma=0), maxit=10)
  tr = tuneParams(lrn1, binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="SANN", 
    start=list(cp=0.05, minsplit=5), maxit=10)
  expect_error(tuneParams(lrn2, binaryclass.task, res, par.set=ps2, control=ctrl))
  
  ctrl = makeTuneControlOptim(method="L-BFGS-B", 
    start=list(C=0, sigma=0), maxit=10)
  tr = tuneParams(lrn1, binaryclass.task, res, par.set=ps1, control=ctrl)
  ctrl = makeTuneControlOptim(method="L-BFGS-B", 
    start=list(cp=0.05, minsplit=5), maxit=10)
  tr = tuneParams(lrn2, binaryclass.task, res, par.set=ps2, control=ctrl)
  
  expect_error(tuneParams(lrn2, multiclass.task, res, par.set=ps3, control=ctrl))
})

