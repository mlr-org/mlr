context("submodel")

test_that("classification submodel", {
  # binary classification
  a = lapply(c("classif.randomForest", "classif.gbm"), function(x) { 
    lrn = makeLearner(x)
    mod = train(lrn, binaryclass.task, subset = binaryclass.train.inds)
    set.seed(123)
    pred = predict(mod, newdata = binaryclass.test)
    predsub = predict(mod, newdata = binaryclass.test, submodel.value = 2)
    expect_true(any(pred$data$response != predsub$data$response))
    expect_true(performance(pred, mmce) != performance(predsub, mmce))
    suppressAll(expect_error(predict(mod, newdata = binaryclass.test, submodel.value = 50000L)))
  })

  # multiclass classification
  a = lapply(c("classif.randomForest", "classif.gbm"), function(x) { 
    lrn = makeLearner(x)
    mod = train(lrn, multiclass.task, subset = multiclass.train.inds)
    set.seed(123)
    pred = predict(mod, newdata = multiclass.test)
    predsub = predict(mod, newdata = multiclass.test, submodel.value = 2)
    expect_true(any(pred$data$response != predsub$data$response))
    expect_true(performance(pred, mmce) != performance(predsub, mmce))
    suppressAll(expect_error(predict(mod, newdata = multiclass.test, submodel.value = 50000L)))
  })
})

test_that("regression submodel", {
  a = lapply(c("regr.randomForest", "regr.gbm"), function(x) { 
    lrn = makeLearner(x)
    mod = train(lrn, regr.task, subset = regr.train.inds)
    set.seed(123)
    pred = predict(mod, newdata = regr.test)
    predsub = predict(mod, newdata = regr.test, submodel.value = 2)
    expect_true(any(pred$data$response != predsub$data$response))
    expect_true(performance(pred, mse) != performance(predsub, mse))
    suppressAll(expect_error(predict(mod, newdata = regr.test, submodel.value = 50000L)))
  })
})
