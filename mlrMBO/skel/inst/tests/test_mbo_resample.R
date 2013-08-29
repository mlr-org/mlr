context("mbo resample")

test_that("mbo works with resampling", {
  f = makeMBOFunction(function(x) sum(x^2))
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=0, upper=1)
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters=5, infill.opt.random.points=10, resample.at=c(1,3))
  or = mbo(f, ps, des=NULL, learner, ctrl)
  x = or$resample
  expect_true(is.list(x) && length(x) == 2)
  expect_true(is.numeric(x[[1]]) && is.numeric(x[[2]]))
  expect_equal(names(x), c("1", "3"))
})
