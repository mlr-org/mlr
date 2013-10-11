context("dependent params")

#FIXME rf does not extrapolate enough
test_that("dependent params", {
  f = function(x) {
    x = removeMissingValues(x)
    x$x0 + ifelse(x$foo == "a", x$x1, as.numeric(x$x2 == "v2"))
  }

  ps = makeParamSet(
    makeDiscreteParam("foo", values = c("a", "b")),
    makeNumericParam("x0", lower=3, upper=4),
    makeNumericParam("x1", lower=-2, upper=2, requires=quote(foo == "a")),
    makeDiscreteParam("x2", values=c("v1", "v2"), requires=quote(foo == "b"))
  )
  
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(init.design.points=50, iters=20, infill.opt.random.points=400)
  or = mbo(f, ps, learner=learner, control=ctrl, show.info=TRUE)
  expect_true(!is.na(or$y))
  x = or$x
  print(x)
  expect_true(x$foo == "a" && x$x0 < 3.2 && x$x1 < -1.8)
  
})
