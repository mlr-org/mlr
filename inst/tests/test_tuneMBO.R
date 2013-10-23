context("tuneMBO")

# FIXME remove if mbo is on cran

if (isExpensiveExampleOk()) {

test_that("tuneMBO", {
  library(mlrMBO)
  res = makeResampleDesc("Subsample", iters=4)
  ps1 = makeParamSet(
    makeIntegerParam("ntree", lower=1, upper=5)
  )
  
  n1 = 10; n2=2;
  mbo.ctrl = makeMBOControl(init.design.points=n1, iters=n2)
  ctrl = makeTuneControlMBO(learner=makeLearner("regr.randomForest"), mbo.control=mbo.ctrl)
  tr1 = tuneParams(makeLearner("classif.randomForest"), multiclass.task, res, par.set=ps1, control=ctrl)
  expect_equal(getOptPathLength(tr1$opt.path), n1+n2)
  expect_equal(dim(as.data.frame(tr1$opt.path)), c(n1+n2, 1+1+2))
  
  ps2 = makeParamSet(
    makeIntegerParam("ntree", lower=10, upper=50),
    makeNumericVectorParam("cutoff", len=3, lower=0.001, upper=1, trafo=function(x) 0.9*x/sum(x)) 
  )
  tr2 = tuneParams(makeLearner("classif.randomForest"), multiclass.task, res, par.set=ps2, control=ctrl)
  expect_equal(getOptPathLength(tr2$opt.path), n1+n2)
})


}

