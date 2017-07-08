context("classif_multinom")

test_that("classif_multinom", {
  requirePackagesOrSkip("nnet", default.method = "load")

  set.seed(getOption("mlr.debug.seed"))
  capture.output({m = nnet::multinom(formula = multiclass.formula, data = multiclass.train)})

  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = multiclass.test)

  testSimple("classif.multinom", multiclass.df, multiclass.target, multiclass.train.inds, p)

  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = multiclass.test, type = "probs")
  testProb("classif.multinom", multiclass.df, multiclass.target, multiclass.train.inds, p)

  tt = nnet::multinom
  tp = function(model, newdata) predict(model, newdata)

  testCV("classif.multinom", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)

  # test multinom for 2 classes
  wl = makeLearner("classif.multinom", predict.type = "prob")
  m = train(wl, binaryclass.task)
  p = predict(m, newdata = binaryclass.df)
  rr = p$data$response
  pp = getPredictionProbabilities(p)
  i = as.integer(pp < 0.5) + 1
  labs = as.factor(getTaskClassLevels(binaryclass.task)[i])
  expect_equal(rr, labs)
})
