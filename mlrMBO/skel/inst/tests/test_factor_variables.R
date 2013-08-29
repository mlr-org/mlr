context("factor variables")

# FIXME: add a text with discrete and factor vecs

test_that("mbo works with factor variables", {
  f = function(x) {
    if (x$foo == "a")
      sum(x$x^2)
    else
      sum(x$x^2) + 10
  }

  ps = makeParamSet(
    makeDiscreteParam("foo", values = c("a", "b")),
    makeNumericVectorParam("x", len=2, lower=-2, upper=2)
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(init.design.points=20, iters=5, infill.opt.random.points=100, save.model.at=c(0,5))
  or = mbo(f, ps, learner=learner, control=ctrl, show.info=FALSE)
  expect_true(!is.na(or$y))
  expect_equal(or$y, f(or$x))
  expect_equal(getOptPathLength(or$opt.path), 20+5)
})
