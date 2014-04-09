context("measures")

test_that("measures", {
  ct = binaryclass.task

  mymeasure = makeMeasure(id="foo", minimize=TRUE, classif=TRUE, regr=TRUE, allowed.pred.types=c("response", "prob"),
    fun=function(task, model, pred, extra.args) {
      tt = pred
      1
    }
  )
  ms = list(mmce, acc, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1, mymeasure)

  res = makeResampleDesc("CV", iters=3)
  lrn = makeLearner("classif.rpart")
  mod = train(lrn, task=ct, subset=binaryclass.train.inds)
  pred = predict(mod, task=ct, subset=binaryclass.test.inds)
  perf = performance(pred, measures=ms)

  r = resample(lrn, ct, res, measures=ms)
  expect_equal(names(r$measures.test),
    c("iter", "mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))

  # Test multiclass auc
  lrn = makeLearner("classif.randomForest",predict.type="prob")
  mod = train(lrn, task=multiclass.task, subset=multiclass.train.inds)
  pred = predict(mod, task=multiclass.task, subset=multiclass.test.inds)
  perf = performance(pred, measures=multiclass.auc)
  expect_is(perf, "numeric")

})
