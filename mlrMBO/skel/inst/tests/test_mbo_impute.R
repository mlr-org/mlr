context("mbo impute")

test_that("mbo works with failures", {
  f1 = makeMBOFunction(function(x) {
    y = sum(x^2)
    if (y < 5)
      return(NA)
    return(y)
  })
  f2 = makeMBOFunction(function(x) {
    y = sum(x^2)
    if (y < 5)
      stop("foo")
    return(y)
  })
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=0, upper=3)
  )
  learner = makeLearner("regr.randomForest")
  
  ctrl = makeMBOControl(iters=20, infill.opt.random.points=500)
  expect_error(mbo(f1, ps, des=NULL, learner, ctrl, show.info=FALSE), "Infeasible y")
  ctrl = makeMBOControl(iters=20,  infill.opt.random.points=500, impute=function(x, y, opt.path) 0)
  mbo(f1, ps, des=NULL, learner, ctrl, show.info=FALSE)
  ctrl = makeMBOControl(iters=50,  infill.opt.random.points=500)
  expect_error(mbo(f2, ps, des=NULL, learner, ctrl, show.info=FALSE), "foo")
  ctrl = makeMBOControl(iters=50,  infill.opt.random.points=500, impute=function(x, y, opt.path) 0, impute.errors=TRUE)
  mbo(f2, ps, des=NULL, learner, ctrl, show.info=FALSE)
})
