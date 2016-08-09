context("Learner")

test_that("Learner", {
  wl = makeLearner("classif.rpart", minsplit = 3)
  expect_equal(wl$type, "classif")
  expect_equal(wl$id, "classif.rpart")
  expect_true(is.character(wl$properties))
  expect_true(length(wl$properties) >= 6)

  wl = makeLearner("regr.lm")
  expect_equal(wl$type, "regr")
  expect_equal(wl$id, "regr.lm")
  expect_true(is.character(wl$properties))

  expect_error(makeLearner("classif.lvq1", predict.type = "prob"), "Trying to predict probs, but")
  expect_error(makeLearner("regr.lm", predict.type = "prob"), "'predict.type'")
  wl = makeLearner("classif.lvq1")
  expect_error(setPredictType(wl, "prob"), "Trying to predict probs, but")

  wl = makeLearner("regr.lm", config = list(on.learner.error = "quiet"))
  expect_equal(wl$config$on.learner.error, "quiet")

  expect_error(makeLearner("classif.lda", predict.threshold = 1, "'prob' must hold"))
})


test_that("allow expressions", {
  ## expressions within parameter sets
  lrn1 = makeLearner("classif.randomForest")
  lrn2 = evaluateLearner(lrn = lrn1, task = binaryclass.task)
  x1 = lrn1$par.set$pars$mtry$default
  x2 = lrn2$par.set$pars$mtry$default
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, floor(sqrt(ncol(binaryclass.df))))

  x1 = lrn1$par.set$pars$classwt$len
  x2 = lrn2$par.set$pars$classwt$len
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, 2)

  x1 = lrn1$par.set$pars$cutoff$len
  x2 = lrn2$par.set$pars$cutoff$len
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(x2, 2)
  
  ## expressions within hyperparameters
  lrn1 = makeLearner("classif.rpart", minsplit = expression(k * p))
  lrn2 = evaluateLearner(lrn = lrn1, task = binaryclass.task)
  x1 = lrn1$par.vals$minsplit
  x2 = lrn2$par.vals$minsplit
  expect_true(is.expression(x1))
  expect_true(!is.expression(x2))
  expect_equal(lrn2$par.vals$minsplit, 2 * getTaskNFeats(binaryclass.task))
})
