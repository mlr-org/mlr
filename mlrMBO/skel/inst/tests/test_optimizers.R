context("infill optimizers")

test_that("infill optimizers", {
  f = makeMBOFunction(function(x) sum(x^2))
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=-10, upper=10) 
  )
  mycontrol = function(opt, restarts) {
    makeMBOControl(init.design.points=20, iters=5, infill.crit="mean", infill.opt=opt,
      infill.opt.cmaes.control = list(maxit=10))
  }
  mycheck = function(or) {
    expect_equal(getOptPathLength(or$opt.path), 25)
    expect_true(!is.na(or$y))
    expect_true(or$y < 1)
  }
  
  learner = makeLearner("regr.km", nugget.estim=TRUE)
  ctrl = mycontrol("cmaes", 1)
  or = mbo(f, ps, NULL, learner, ctrl, show.info=FALSE)
  mycheck(or)
  ctrl = mycontrol("cmaes", 2)
  or = mbo(f, ps, NULL, learner, ctrl, show.info=FALSE)
  mycheck(or)
})
          

