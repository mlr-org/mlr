context("getClassWeightParam")

test_that("getClassWeightParam", {
  
  #basic learner
  lrn = makeLearner("classif.ksvm")
  ps = lrn$par.set$pars[[lrn$class.weights.param]]
  expect_equal(ps, getClassWeightParam(lrn))
  
  #basic learner without class weights
  lrnErr = makeLearner("classif.rpart")
  expect_error(getClassWeightParam(lrnErr))
  
  #wrapped learner
  lrnWrap = makeBaggingWrapper(lrn)
  expect_equal(ps, getClassWeightParam(lrn))
  
  #model multiplexer with at least 1 learner without class.weight prop
  modMult = makeModelMultiplexer(list(lrn, makeLearner("classif.rpart")))
  expect_error(getClassWeightParam(modMult))

  #model multiplexer with all learners with class.weight prop
  lrnRf = makeLearner("classif.randomForest")
  psRf = lrnRf$par.set$pars[[lrnRf$class.weights.param]]
  modMult = makeModelMultiplexer(list(lrn, lrnRf))
  res = list("classif.ksvm" = ps, "classif.randomForest" = psRf)
  expect_equal(res, getClassWeightParam(modMult))
  
})