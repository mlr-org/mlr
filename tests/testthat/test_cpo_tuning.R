
context("tuneCPO")

test_that("tune cpo", {
  lrn = cpoScale() %>>% makeLearner("classif.ctree")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeLogicalParam("scale.scale"),
    makeLogicalParam("scale.center")
  )

  ctrl = makeTuneControlRandom(maxit = 10)
  tr = tuneParams(lrn, binaryclass.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 10)
  expect_true(!is.na(tr$y))
})

test_that("tune cpo, hybrid parameter set", {
  lrn = cpoScale() %>>% makeLearner("classif.ctree")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeLogicalParam("stump"),
    makeLogicalParam("scale.scale"),
    makeLogicalParam("scale.center")
  )

  ctrl = makeTuneControlRandom(maxit = 10)
  tr = tuneParams(lrn, binaryclass.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 10)
  expect_true(!is.na(tr$y))
})
