context("tuneMIES")

test_that("tuneMIES", {
  res = makeResampleDesc("Holdout")
  ps1 = makeParamSet(
    makeNumericVectorParam("cutoff", length=2, lower=0.001, upper=1, trafo=function(x) as.numeric(x / sum(x))), 
    makeIntegerParam("ntree", lower=10, upper=1000), 
    makeLogicalParam("replace")
  )
  
  ctrl = makeTuneControlMies(budget=20, lambda=5)
  lrn = makeLearner("classif.randomForest")
  tr1 = tune(lrn, binaryclass.task, res, par.set=ps1, control=ctrl)
  expect_equal(getOptPathLength(tr1@opt.path), 10)
  expect_equal(dim(as.data.frame(tr1@opt.path)), c(10, 3+1+2))
})
  