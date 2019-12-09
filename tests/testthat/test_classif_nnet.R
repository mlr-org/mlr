context("classif_nnet")

test_that("classif_nnet", {
  requirePackagesOrSkip("nnet", default.method = "load")

  capture.output({
    m = nnet::nnet(multiclass.formula, size = 3, data = multiclass.train)
    p = as.factor(predict(m, newdata = multiclass.test, type = "class"))
    p2 = predict(m, newdata = multiclass.test, type = "raw")
    set.seed(getOption("mlr.debug.seed"))
    m = nnet::nnet(binaryclass.formula, size = 3, data = binaryclass.train)
    # for the binaryclass task the mlr positive class is not the same as the ref
    # class of nnet
    p3 = 1 - predict(m, newdata = binaryclass.test, type = "raw")[, 1]
  })
  testSimple("classif.nnet", multiclass.df, multiclass.target,
    multiclass.train.inds, p, parset = list())
  testProb("classif.nnet", multiclass.df, multiclass.target,
    multiclass.train.inds, p2, parset = list())
  testProb("classif.nnet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p3, parset = list())

  tt = function(formula, data, subset = 1:150, ...) {
    nnet::nnet(formula, data = data[subset, ], size = 7, maxit = 50)
  }
  tp = function(model, newdata) as.factor(predict(model, newdata, type = "class"))

  testCV("classif.nnet", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp,
    parset = list(size = 7, maxit = 50))

  # ## make sure that nnet yields the same results independent of predict.type
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  lrn = makeLearner("classif.nnet", trace = FALSE, size = 1,
    predict.type = "prob")
  mod = train(lrn, task = task)
  pred1 = predict(mod, task = task)
  lrn = makeLearner("classif.nnet", trace = FALSE, size = 1)
  mod = train(lrn, task = task)
  pred2 = predict(mod, task = task)
  expect_equal(pred1$data$response, pred2$data$response)
})
