context("hyperpars")

test_that("hyperpars", {
  lrn = makeLearner("classif.rpart", minsplit = 10)
  expect_equal(getHyperPars(lrn), list(xval = 0, minsplit = 10))

  m = train(lrn, task = multiclass.task)
  expect_true(!inherits(m, "FailureModel"))
  expect_equal(getHyperPars(m$learner), list(xval = 0, minsplit = 10))

  # test equality after removing using removeHyperPars
  lrn = makeLearner("classif.J48", C = 0.5)
  expect_identical(getHyperPars(makeLearner("classif.J48")),
    getHyperPars(removeHyperPars(lrn, "C")))

  # test a more complex param object
  lrn = makeLearner("classif.ksvm", class.weights = c(setosa = 1,
    versicolor = 2, virginica = 3))
  m = train(lrn, task = multiclass.task)

  # check warnings
  mlr.opts = getMlrOptions()
  configureMlr(on.par.without.desc = "warn", show.learner.output = FALSE)
  expect_warning(makeLearner("classif.rpart", foo = 1), "Setting parameter foo without")

  configureMlr(on.par.without.desc = "quiet")
  expect_warning(makeLearner("classif.rpart", foo = 1), NA)
  configureMlr(show.learner.output = FALSE)
  do.call(configureMlr, mlr.opts)
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

  # now remove all hyperpars using a wrapped wrapper
  lrn = makeOversampleWrapper(makeFilterWrapper(makeLearner("classif.qda", nu = 2),
    fw.perc = 0.5), osw.rate = 1)
  lrn1 = removeHyperPars(lrn, ids = names(getHyperPars(lrn)))
  expect_true(length(getHyperPars(lrn1)) == 0)
})

test_that("setting 'when' works for hyperpars", {
  lrn = makeLearner("regr.__mlrmocklearners__4", p1 = 1, p2 = 2, p3 = 3)
  hps = getHyperPars(lrn)
  expect_equal(hps, list(p1 = 1, p2 = 2, p3 = 3))
  # model stores p1 + p3 in fit, adds p2,p3 in predict to this (so it predicts constant val)
  m = train(lrn, regr.task)
  expect_equal(m$learner.model, list(foo = 1 + 3))
  p = predict(m, regr.task)
  expect_equal(p$data$response, rep(1 + 2 + 2 * 3, getTaskSize(regr.task)))
})

test_that("fuzzy matching works for mistyped hyperpars", {
  msg = "classif.ksvm: Setting parameter sigm without available description object!\nDid you mean one of these hyperparameters instead: sigma fit type\nYou can switch off this check by using configureMlr!"
  mlr.opts = getMlrOptions()

  # test if config arg works properly in combination with show.info
  cq = list(on.par.without.desc = "quiet")
  cw = list(on.par.without.desc = "warn")
  cs = list(on.par.without.desc = "stop")
  # never print message when quiet
  expect_silent(makeLearner("classif.ksvm", config = cq, sigm = 1))
  configureMlr(on.par.without.desc = "quiet")
  expect_silent(makeLearner("classif.ksvm", sigm = 1))

  # print message and warn
  expect_warning(makeLearner("classif.ksvm", config = cw, sigm = 1), msg)
  configureMlr(on.par.without.desc = "warn")
  expect_warning(makeLearner("classif.ksvm", sigm = 1), msg)

  # print message and error
  expect_error(makeLearner("classif.ksvm", config = cs, sigm = 1), msg)
  configureMlr(on.par.without.desc = "stop")
  expect_error(makeLearner("classif.ksvm", sigm = 1), msg)

  # docu says: for warn and quiet parameter is passed, check if this is true
  lrn = makeLearner("classif.ksvm",
    config = list(on.par.without.desc = "quiet"))
  expect_equal(getHyperPars(setHyperPars(lrn, sigm = 1))$sigm, 1)
  lrn = makeLearner("classif.ksvm",
    config = list(on.par.without.desc = "warn"))
  expect_warning(expect_equal(getHyperPars(setHyperPars(lrn, sigm = 1))$sigm, 1))

  do.call(configureMlr, mlr.opts)
})

test_that("options are respected", {
  # with local option

  lrn = makeLearner("classif.__mlrmocklearners__2")
  expect_error(setHyperPars(lrn, beta = 1), "available description object")
  lrn = makeLearner("classif.__mlrmocklearners__2", config = list(on.par.without.desc = "warn"))
  expect_warning(setHyperPars(lrn, beta = 1), "available description object")
  lrn = makeLearner("classif.__mlrmocklearners__2", config = list(on.par.without.desc = "quiet"))
  expect_is(setHyperPars(lrn, beta = 1), "Learner")

  lrn = makeLearner("classif.__mlrmocklearners__2")
  expect_error(setHyperPars(lrn, alpha = 2), "feasible")
  lrn = makeLearner("classif.__mlrmocklearners__2", config = list(on.par.out.of.bounds = "warn"))
  expect_warning(setHyperPars(lrn, alpha = 2), "feasible")
  lrn = makeLearner("classif.__mlrmocklearners__2", config = list(on.par.out.of.bounds = "quiet"))
  expect_is(setHyperPars(lrn, alpha = 2), "Learner")


  # with global option
  mlr.opts = getMlrOptions()

  lrn = makeLearner("classif.__mlrmocklearners__2")
  configureMlr(on.par.without.desc = "quiet")
  expect_is(setHyperPars(lrn, beta = 1), "Learner")
  configureMlr(on.par.out.of.bounds = "quiet")
  expect_is(setHyperPars(lrn, alpha = 2), "Learner")

  do.call(configureMlr, mlr.opts)
})
