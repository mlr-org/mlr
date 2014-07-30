context("hyperpars")

test_that("hyperpars", {
  lrn = makeLearner("classif.rpart", minsplit=10)
  expect_equal(getHyperPars(lrn), list(xval=0, minsplit=10))

  m = train(lrn, task=multiclass.task)
  expect_true(!inherits(m, "FailureModel"))
  expect_equal(getHyperPars(m$learner), list(xval=0, minsplit=10))

  # test a more complex param object
  lrn = makeLearner("classif.ksvm", class.weights=c(setosa=1, versicolor=2, virginica=3))
  m = train(lrn, task=multiclass.task)

  # check warnings
  configureMlr(on.par.without.desc="warn", show.learner.output=FALSE)
  expect_warning(makeLearner("classif.rpart", foo=1), "Setting parameter foo without")
  configureMlr(on.par.without.desc="quiet")
  expect_that(makeLearner("classif.rpart", foo=1), not(gives_warning()))
  configureMlr(show.learner.output=FALSE)
})


test_that("removing par settings works", {
  lrn = makeLearner("classif.qda")
  expect_error(removeHyperPars(lrn, "minsplit"), "Trying to remove")
  expect_error(removeHyperPars(lrn, "xxx"), "Trying to remove")
  lrn2 = setHyperPars(lrn, method = "mve", nu = 7)
  lrn3 = removeHyperPars(lrn2, "method")
  expect_equal(getHyperPars(lrn3), list(nu = 7))

  # now with wrapper
  lrn = makeBaggingWrapper(makeLearner("classif.qda"))
  lrn2 = setHyperPars(lrn, method = "mve", bw.iters = 9)
  lrn3 = removeHyperPars(lrn2, "method")
  expect_equal(getHyperPars(lrn3), list(bw.iters = 9))
  lrn3 = removeHyperPars(lrn2, "bw.iters")
  expect_equal(getHyperPars(lrn3), list(method = "mve"))

})


