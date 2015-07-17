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


test_that("measures with same id still work", {
  m1 = mmce
  m2 = acc
  m1$id = m2$id = "foo"
  r = holdout("classif.rpart", iris.task, measures = list(m1, m2))
  expect_true(r$aggr[1L] < 0.2 && r$aggr[2L] > 0.8)
})

test_that("ber with faulty model produces NA", {
  data = iris; data[,1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_true(is.na(r$aggr))
})

test_that("db with single cluster doesn't give warnings", {
  expect_that(crossval("cluster.kmeans", agri.task), not(gives_warning()))
})

test_that("mcc is implemented correctly", { # see issue 363
  r = holdout("classif.rpart", sonar.task, measure = mcc)
  p = as.data.frame(r$pred)
  cm = getConfMatrix(r$pred)[1:2, 1:2]

  # taken from psych::phi. the phi measure is another name for mcc
  r.sum = rowSums(cm)
  c.sum = colSums(cm)
  total = sum(r.sum)
  r.sum = r.sum / total
  c.sum = c.sum / total
  v = prod(r.sum, c.sum)
  phi = (cm[1, 1] / total - c.sum[1] * r.sum[1])/sqrt(v)
  expect_equal(r$aggr[[1]], phi[[1L]])
})

test_that("listMeasures", {
  mycheck = function(type) {
    xs = listMeasures(type, create = TRUE)
    expect_true(is.list(xs) && length(xs) > 0L, info = type)
    expect_true(all(vlapply(xs, inherits, what = "Measure")), info = type)
  }
  mycheck("classif")
  mycheck("regr")
  mycheck("cluster")
  mycheck("surv")
  mycheck("costsens")
  mycheck("multilabel")
})

