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
  for (m in ms) {
    perf = performance(pred, measure=m)
  }
	
  r = resample(lrn, ct, res, measures=ms)
	expect_equal(names(r$measures.test), c("iter", "mmce", "acc", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr", "fnr", "ppv", "npv", "mcc", "f1", "foo"))
})
