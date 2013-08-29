context("mbo")

test_that("mbo works with rf", {
  f = makeMBOFunction(function(x) sum(x^2))
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeNumericParam("x2", lower=-1, upper=2) 
  )
  des = generateDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100, save.model.at=c(0,5))
  or = mbo(f, ps, des, learner, ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(or$y, f(or$x))
  expect_equal(getOptPathLength(or$opt.path), 15)
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(ps$pars))
  expect_true(is(or$models[[1]]$learner, "regr.randomForest"))
  expect_equal(length(or$models[[1]]$subset), 10)
  expect_true(is(or$models[[2]]$learner, "regr.randomForest"))
  expect_equal(length(or$models[[2]]$subset), 15)
  
  # check errors
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100, infill.crit="ei")
  expect_error(mbo(f, ps, des, learner, ctrl), "must be set to 'se'")
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100)
  
  f2=makeMBOFunction(function(x) x^2)
  expect_error(mbo(f2, ps, des, learner, ctrl), "univariate")
  
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100)
  learner = makeLearner("classif.randomForest")
  expect_error(mbo(f, ps, des, learner, ctrl), "mbo requires regression learner")
  learner = makeLearner("regr.randomForest")
  # check trafo
  ps = makeParamSet(
    makeNumericParam("x1", lower=-10, upper=10, trafo=function(x) abs(x)) 
  )
  des = generateDesign(10, par.set=ps)
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  or = mbo(f, ps, des, learner, ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  
  # discrete par
  f = function(x) if(x[[3]]=="a") x[[1]]^2+x[[2]]^2 else x[[1]]^2+x[[2]]^2 + 20 
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeIntegerParam("x2", lower=-1, upper=2), 
    makeDiscreteParam("x3", values=c("a", "b")) 
  )
  des = generateDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100)
  or = mbo(f, ps, des, learner, ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.integer(df$x2))
  expect_true(is.character(df$x3))
  expect_true(is.numeric(df$y))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(ps$pars))

  # check best.predicted
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=100, final.method="best.predicted")
  or = mbo(f, ps, des, learner, ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)

  ctrl = makeMBOControl(init.design.points=10, iters=5, infill.opt.random.points=100)
  or = mbo(f, ps, des=NULL, learner, ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  expect_equal(names(or$x), names(ps$pars))
  
  # check cmaes
  f = function(x) sum(x[[1]]^2) + (2 - x[[2]])^2
  
  ps = makeParamSet(
    makeNumericVectorParam("v", lower=-5, upper=5, len=2), 
    makeNumericParam("w", lower=-5, upper=5) 
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(init.design.points=10, iters=3, 
    infill.opt="cmaes", infill.opt.cmaes.control=list(maxit=2))
  or = mbo(f, ps, des=NULL, learner, ctrl, show.info = FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 10 + 3)
  ctrl = makeMBOControl(init.design.points=10, iters=3,
    infill.opt="cmaes", infill.opt.cmaes.control=list(maxit=2),
    final.method="best.predicted")
  or = mbo(f, ps, des=NULL, learner, ctrl, show.info=FALSE)
  expect_equal(getOptPathLength(or$opt.path), 10 + 3)
})

test_that("mbo works with logicals", {
  f = function(x) {
    if (x$a)
      sum(x$b^2)
    else
      sum(x$b^2 + 10)
  }
          
  ps = makeParamSet(
    makeLogicalParam("a"), 
    makeNumericVectorParam("b", len=2, lower=-3, upper=3) 
  )
  learner = makeLearner("regr.randomForest", ntree=50)
  ctrl = makeMBOControl(init.design.points=10, iters=10, infill.opt.random.points=10000)
  or = mbo(f, ps, learner=learner, control=ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 20)
  expect_equal(or$x$a, TRUE)
  expect_true(sum(or$x$b) < 0.1)
})  

