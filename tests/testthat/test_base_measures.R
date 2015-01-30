context("measures")

test_that("measures", {
  ct = binaryclass.task

  mymeasure = makeMeasure(id = "foo", minimize = TRUE, properties = c("classif", "classif.multi", "regr", "predtype.response", "predtype.prob"),
    fun = function(task, model, pred, feats, extra.args) {
      tt = pred
      1
    }
  )
  ms = list(mmce, acc, bac, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1, mymeasure)

  lrn = makeLearner("classif.rpart")
  mod = train(lrn, task = ct, subset = binaryclass.train.inds)
  pred = predict(mod, task = ct, subset = binaryclass.test.inds)
  perf = performance(pred, measures = ms)

  rdesc = makeResampleDesc("Holdout", split = 0.2)
  r = resample(lrn, ct, rdesc, measures = ms)
  expect_equal(names(r$measures.train),
    c("iter", "mmce", "acc", "bac", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))
  expect_equal(names(r$measures.test),
    c("iter", "mmce", "acc", "bac", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))

  # test that measures work for se
  ms = list(mse, timetrain, timepredict, timeboth, featperc)
  lrn = makeLearner("regr.lm", predict.type = "se")
  mod = train(lrn, task = regr.task, subset = regr.train.inds)
  pred = predict(mod, task = regr.task, subset = regr.test.inds)
  perf = performance(pred, measures = ms, model = mod)
  expect_is(perf, "numeric")

  # Test multiclass auc
  lrn = makeLearner("classif.randomForest", predict.type = "prob")
  mod = train(lrn, task = multiclass.task, subset = multiclass.train.inds)
  pred = predict(mod, task = multiclass.task, subset = multiclass.test.inds)
  perf = performance(pred, measures = multiclass.auc)
  expect_is(perf, "numeric")

  # test survival measure
  ms = list(cindex)
  lrn = makeLearner("surv.coxph")
  mod = train(lrn, task = surv.task, subset = surv.train.inds)
  pred = predict(mod, task = surv.task, subset = surv.test.inds)
  perf = performance(pred, measures = ms)
  expect_is(perf, "numeric")
})
