context("ModelMultiplexer")


test_that("makeModelMultiplexerParamSet works", {
  bls = list(
    makeLearner("classif.ksvm"),
    makeLearner("classif.randomForest")
  )

  lrn = makeModelMultiplexer(bls)

  ps1 = makeModelMultiplexerParamSet(lrn,
    makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeIntegerParam("ntree", lower = 1L, upper = 500L)
  )

  ps2 = makeModelMultiplexerParamSet(lrn,
    classif.ksvm = makeParamSet(makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x)),
    classif.randomForest = makeParamSet(makeIntegerParam("ntree", lower = 1L, upper = 500L))
  )

  ps3 = makeParamSet(
    makeDiscreteParam("selected.learner", values = extractSubList(bls, "id")),
    makeNumericParam("classif.ksvm.sigma", lower=-10, upper = 10, trafo = function(x) 2^x,
      requires = quote(selected.learner == "classif.ksvm")),
    makeIntegerParam("classif.randomForest.ntree", lower = 1L, upper = 500L,
      requires = quote(selected.learner == "classif.randomForest"))
  )

  expect_equal(ps1, ps2)
  expect_equal(ps2, ps3)
  expect_equal(ps1, ps3)
})

# this is more or less a test for BaseEnsemble, that hyperpars work and so on
test_that("ModelMultiplexer basic stuff works", {
  lrn = makeModelMultiplexer(c("classif.lda", "classif.rpart"))
  expect_equal(class(lrn), c("ModelMultiplexer", "BaseEnsemble", "Learner"))

  # check hyper par setting and so on
  lrn2 = setHyperPars(lrn, selected.learner = "classif.rpart", classif.rpart.minsplit = 10000L)
  xs = getHyperPars(lrn2)
  expect_true(setequal(names(xs), c("selected.learner", "classif.rpart.minsplit", "classif.rpart.xval")))
  expect_equal(xs$classif.rpart.minsplit, 10000L)
  mod = train(lrn2, task = binaryclass.task)
  expect_equal(getLearnerModel(mod, more.unwrap = TRUE)$control$minsplit, 10000L)

  # check removal
  lrn3 = removeHyperPars(lrn2, "classif.rpart.minsplit")
  xs = getHyperPars(lrn3)
  expect_true(setequal(names(xs), c("selected.learner", "classif.rpart.xval")))

  # check predict.type
  lrn2 = setPredictType(lrn, "prob")
  mod = train(lrn2, task = binaryclass.task)
  p = predict(mod, task = binaryclass.task)
  getPredictionProbabilities(p)
})

test_that("FailureModel works", {
  lrn = list(
    makeLearner("classif.__mlrmocklearners__2", config = list(on.learner.error = "warn")),
    makeLearner("classif.rpart", config = list(on.learner.error = "warn"))
  )
  lrn = makeModelMultiplexer(lrn)

  lrn = setHyperPars(lrn, classif.__mlrmocklearners__2.alpha = 1)
  mod = train(lrn, task = iris.task)
  expect_false(isFailureModel(mod))

  lrn = setHyperPars(lrn, classif.__mlrmocklearners__2.alpha = 0)
  expect_warning(mod <- train(lrn, task = iris.task), "foo")
  expect_true(isFailureModel(mod))
})

test_that("ModelMultiplexer tuning", {
  lrn = makeModelMultiplexer(c("classif.knn", "classif.rpart"))
  rdesc = makeResampleDesc("CV", iters = 2L)

  tune.ps = makeModelMultiplexerParamSet(lrn,
    makeIntegerParam("minsplit", lower = 1, upper = 50))
  # tune with random
  ctrl = makeTuneControlRandom(maxit = 4L)
  res = tuneParams(lrn, binaryclass.task, rdesc, par.set = tune.ps, control = ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
  y = getOptPathY(res$opt.path)
  expect_true(!is.na(y) && is.finite(y))
  # tune with irace
  task = subsetTask(binaryclass.task, subset = c(1:20, 150:170))
  ctrl = makeTuneControlIrace(maxExperiments = 40L, nbIterations = 2L, minNbSurvival = 1L)
  res = tuneParams(lrn, task, rdesc, par.set = tune.ps, control = ctrl)
  expect_true(setequal(class(res), c("TuneResult", "OptResult")))
  y = getOptPathY(res$opt.path)
  expect_true(!is.na(y) && is.finite(y))
})

# we had bug here, see issue #609
test_that("ModelMultiplexer inherits predict.type from base learners", {
  base.learners = list(
    makeLearner("classif.ksvm", predict.type = "prob"),
    makeLearner("classif.randomForest", predict.type = "prob")
  )
  learner = makeModelMultiplexer(base.learners)
  expect_equal(learner$predict.type, "prob")
  # now lets see that the next code runs and does not complain about matrix output for
  # base learner predict output
  r = holdout(learner, binaryclass.task)

  # now check that we can tune the threshold
  ps = makeModelMultiplexerParamSet(learner,
    makeDiscreteParam("C", 1),
    makeDiscreteParam("mtry", c(2, 3))
  )
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(tune.threshold = TRUE)
  res = tuneParams(learner, binaryclass.task, resampling = rdesc, par.set = ps, control = ctrl)
})

# we had bug here, see issue #647
test_that("ModelMultiplexer passes on hyper pars in predict", {
  base.learners = list(
    makeLearner("regr.glmnet"),
    makeLearner("regr.rpart")
  )
  learner = makeModelMultiplexer(base.learners)
  expect_equal(learner$predict.type, "response")
  r = holdout(learner, regr.task)
})

