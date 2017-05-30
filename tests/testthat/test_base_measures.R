context("measures")

test_that("measures", {
  ct = binaryclass.task
  options(warn = 2)
  mymeasure = makeMeasure(id = "foo", minimize = TRUE, properties = c("classif", "classif.multi",
    "regr", "predtype.response", "predtype.prob"),
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
    c("iter", "mmce", "acc", "bac", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr",
      "fnr", "ppv", "npv", "mcc", "f1", "foo"))
  expect_equal(names(r$measures.test),
    c("iter", "mmce", "acc", "bac", "tp", "fp", "tn", "fn", "tpr", "fpr", "tnr",
      "fnr", "ppv", "npv", "mcc", "f1", "foo"))

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
  perf = performance(pred, measures = list(multiclass.aunu, multiclass.aunp,
    multiclass.au1u, multiclass.au1p))
  expect_is(perf, "numeric")

  # test survival measure
  ms = list(cindex)
  lrn = makeLearner("surv.coxph")
  mod = train(lrn, task = surv.task, subset = surv.train.inds)
  pred = predict(mod, task = surv.task, subset = surv.test.inds)
  perf = performance(pred, measures = ms)
  expect_is(perf, "numeric")
})

test_that("classif measures do not produce integer overflow", {
  tsk = oversample(subsetTask(pid.task), 1000)
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  ms = listMeasures("classif", create = TRUE)
  r = holdout(lrn, tsk, measures = ms, show.info = FALSE)
  expect_numeric(r$aggr, any.missing = FALSE)
})

test_that("measures with same id still work", {
  m1 = mmce
  m2 = acc
  m1$id = m2$id = "foo"
  r = holdout("classif.rpart", iris.task, measures = list(m1, m2))
  expect_true(r$aggr[1L] < 0.2 && r$aggr[2L] > 0.8)
})

test_that("ber with faulty model produces NA", {
  data = iris; data[, 1] = 1
  lrn = makeLearner("classif.lda", config = list(on.learner.error = "quiet"))
  task = makeClassifTask(data = data, target = "Species")
  r = holdout(lrn, task, measures = ber)
  expect_true(is.na(r$aggr))
})

test_that("db with single cluster doesn't give warnings", {
  expect_warning(crossval("cluster.kmeans", agri.task), NA)
})

test_that("mcc is implemented correctly", { # see issue 363
  r = holdout("classif.rpart", sonar.task, measure = mcc)
  p = as.data.frame(r$pred)
  cm = calculateConfusionMatrix(r$pred)$result[1:2, 1:2]

  # taken from psych::phi. the phi measure is another name for mcc
  r.sum = rowSums(cm)
  c.sum = colSums(cm)
  total = sum(r.sum)
  r.sum = r.sum / total
  c.sum = c.sum / total
  v = prod(r.sum, c.sum)
  phi = (cm[1, 1] / total - c.sum[1] * r.sum[1]) / sqrt(v)
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

test_that("check measure calculations", {
  #tiny datasets for testing
  #features
  var1 = c(1, 2, 3, 4)
  var2 = c(3, 4, 1, 2)
  #for regression
  tar.regr = c(5, 10, 0, 5)
  pred.art.regr = c(4, 11, 0, 4)
  data.regr = data.frame(var1, var2, tar.regr)
  task.regr = makeRegrTask(data = data.regr, target = "tar.regr")
  lrn.regr = makeLearner("regr.rpart")
  mod.regr = train(lrn.regr, task.regr)
  pred.regr = predict(mod.regr, task.regr)
  pred.regr$data$response = pred.art.regr
  #for multiclass
  tar.classif = factor(c(1L, 2L, 0L, 1L))
  pred.art.classif = factor(c(1L, 1L, 0L, 2L))
  data.classif = data.frame(var1, var2, tar.classif)
  task.classif = makeClassifTask(data = data.classif, target = "tar.classif")
  lrn.classif = makeLearner("classif.rpart", predict.type = "prob")
  mod.classif = train(lrn.classif, task.classif)
  pred.classif = predict(mod.classif, task.classif)
  pred.classif$data$response = pred.art.classif
  #for binaryclass
  tar.bin = factor(c(1L, 0L, 0L, 1L))
  pred.art.bin = factor(c(1L, 1L, 0L, 0L))
  data.bin = data.frame(var1, var2, tar.bin)
  task.bin = makeClassifTask(data = data.bin, target = "tar.bin")
  lrn.bin = lrn.classif
  mod.bin = train(lrn.bin, task.bin)
  pred.bin = predict(mod.bin, task.bin)
  pred.bin$data$response = pred.art.bin
  #for multilabel
  tar1.multilabel = c(TRUE, FALSE, FALSE, TRUE)
  tar2.multilabel = c(TRUE, TRUE, FALSE, TRUE)
  pred.art.multilabel = cbind(c(TRUE, FALSE, FALSE, FALSE), c(FALSE, TRUE, FALSE, TRUE))
  data.multilabel = data.frame(var1, var2, tar1.multilabel, tar2.multilabel)
  label.names = c("tar1.multilabel", "tar2.multilabel")
  task.multilabel = makeMultilabelTask(data = data.multilabel, target = label.names)
  lrn.multilabel = makeLearner("multilabel.rFerns")
  mod.multilabel = train(lrn.multilabel, task.multilabel)
  pred.multilabel = predict(mod.multilabel, task.multilabel)
  pred.multilabel$data[, 4:5] = pred.art.multilabel
  #for survival
  time.surv = c(5, 10, 5, 10)
  status.surv = c(TRUE, FALSE, TRUE, FALSE)
  pred.art.surv = c(1, -1, 1, 1)
  data.surv = data.frame(var1, var2, time.surv, status.surv)
  tar.names = c("time.surv", "status.surv")
  task.surv = makeSurvTask(data = data.surv, target = tar.names)
  lrn.surv = makeLearner("surv.coxph")
  # lm does not converge due to small data and warns
  suppressWarnings({
    mod.surv = train(lrn.surv, task.surv)
  })
  pred.surv = predict(mod.surv, task.surv)
  pred.surv$data[, "response"] = pred.art.surv
  #for costsensitive
  tar.costsens = factor(c("a", "b", "c", "a"))
  pred.art.costsens = factor(c("a", "b", "c", "c"))
  data.costsens = data.frame(var1, var2)
  costs = matrix(c(0, 1, 2, 1, 0, 2, 1, 2, 0, 0, 2, 1), nrow = 4L, byrow = TRUE)
  colnames(costs) = levels(tar.costsens)
  rownames(costs) = rownames(data.costsens)
  task.costsens = makeCostSensTask(data = data.costsens, cost = costs)
  lrn.costsens = makeLearner("classif.multinom", trace = FALSE)
  lrn.costsens = makeCostSensWeightedPairsWrapper(lrn.costsens)
  mod.costsens = train(lrn.costsens, task.costsens)
  pred.costsens = predict(mod.costsens, task = task.costsens)
  pred.costsens$data$response = pred.art.costsens
  #for clustering
  pred.art.cluster = c(1L, 1L, 2L, 1L)
  data.cluster = data.frame(var1, var2)
  task.cluster = makeClusterTask(data = data.cluster)
  lrn.cluster = makeLearner("cluster.EM")
  mod.cluster = train(lrn.cluster, task.cluster)
  pred.cluster = predict(mod.cluster, task.cluster)
  pred.cluster$data$response = pred.art.cluster

  #test regression measures

  #sse
  sq.errs = c(5 - 4, 10 - 11, 0 - 0, 5 - 4)^2L
  sse.test = sum(sq.errs)
  sse.perf = performance(pred.regr, measures = sse, model = mod.regr)
  expect_equal(sse.test, sse$fun(pred = pred.regr))
  expect_equal(sse.test, as.numeric(sse.perf))
  #mse
  mse.test = mean(sq.errs)
  mse.perf = performance(pred.regr, measures = mse, model = mod.regr)
  expect_equal(mse.test, mse$fun(pred = pred.regr))
  expect_equal(mse.test, as.numeric(mse.perf))
  #rmse
  rmse.test = sqrt(mse.test)
  rmse.perf = performance(pred.regr, measures = rmse, model = mod.regr)
  expect_equal(rmse.test, rmse$fun(pred = pred.regr))
  expect_equal(rmse.test, as.numeric(rmse.perf))
  #medse
  medse.test = median(sq.errs)
  medse.perf = performance(pred.regr, measures = medse, model = mod.regr)
  expect_equal(medse.test, medse$fun(pred = pred.regr))
  expect_equal(medse.test, as.numeric(medse.perf))
  #sae
  abs.errs = abs(c(5 - 4, 10 - 11, 0 - 0, 5 - 4))
  sae.test = sum(abs.errs)
  sae.perf = performance(pred.regr, measures = sae, model = mod.regr)
  expect_equal(sae.test, sae$fun(pred = pred.regr))
  expect_equal(sae.test, as.numeric(sae.perf))
  #mae
  mae.test = mean(abs.errs)
  mae.perf = performance(pred.regr, measures = mae, model = mod.regr)
  expect_equal(mae.test, mae$fun(pred = pred.regr))
  expect_equal(mae.test, as.numeric(mae.perf))
  #medae
  medae.test = median(abs.errs)
  medae.perf = performance(pred.regr, measures = medae, model = mod.regr)
  expect_equal(medae.test, medae$fun(pred = pred.regr))
  expect_equal(medae.test, as.numeric(medae.perf))
  # rsq
  rsq.test = 1 - (sse.test / sum((tar.regr - mean(tar.regr))^2L))
  rsq.perf = performance(pred.regr, measures = rsq, model = mod.regr)
  expect_equal(rsq.test, rsq$fun(pred = pred.regr))
  expect_equal(rsq.test, as.numeric(rsq.perf))
  expect_equal(1 - ((4 - 5)^2 + (11 - 10)^2 + (0 - 0)^2 + (4 - 5)^2) / ((5 - 5)^2 + (10 - 5)^2 + (0 - 5)^2 + (5 - 5)^2), measureRSQ(c(5, 10, 0, 5), c(4, 11, 0, 4)))
  suppressWarnings({
    expect_equal(NA_real_, measureRSQ(0, 0))
    expect_warning(measureRSQ(0, 0))
    expect_warning(measureRSQ(1, 1))
    expect_warning(measureRSQ(c(1, 1, 1, 1), c(1, 2, 3, 4)))
  })
  expect_silent(measureRSQ(c(1, 1, 1, 0), c(2, 2, 2, 2)))
  # arsq
  arsq.test = 1 - (1 - rsq.test) * (2L / (4L - 2L - 1L))
  arsq.perf = performance(pred.regr, measures = arsq,
    model = mod.regr)
  expect_equal(arsq.test, arsq$fun(pred = pred.regr, model = mod.regr))
  expect_equal(arsq.test, as.numeric(arsq.perf))
  task.regr.arsq = subsetTask(task = task.regr, subset = 1:3)
  mod.regr.arsq = train(lrn.regr, task.regr.arsq)
  pred.regr.arsq = predict(mod.regr.arsq, task.regr.arsq)
  suppressWarnings({
    expect_equal(NA_real_, as.numeric(performance(pred.regr.arsq, measures = arsq,
      model = mod.regr.arsq)))
    expect_warning(performance(pred.regr.arsq, measures = arsq,
      model = mod.regr.arsq))
  })
  # expvar
  expvar.test = sum((pred.art.regr - mean(tar.regr))^2L) / sum((tar.regr - mean(tar.regr))^2L)
  expvar.perf = performance(pred.regr, measures = expvar, model = mod.regr)
  expect_equal(expvar.test, expvar$fun(pred = pred.regr))
  expect_equal(expvar.test, as.numeric(expvar.perf))
  expect_equal(sum((1 - 3)^2 + (2 - 3)^2 + (3 - 3)^2 + (4 - 3)^2 + (5 - 3)^2) / sum((5 - 3)^2 + (4 - 3)^2 + (3 - 3)^2 + (2 - 3)^2 + (1 - 3)^2), measureEXPVAR(5:1, 1:5))
  suppressWarnings({
    expect_equal(NA_real_, measureEXPVAR(0, 0))
    expect_warning(measureEXPVAR(0, 0))
    expect_warning(measureEXPVAR(c(1, 1, 1, 1), c(1, 2, 3, 4)))
  })
  expect_silent(measureEXPVAR(c(1, 1, 1, 0), c(2, 2, 2, 2)))
  # rrse
  rrse.test = sqrt(sum((pred.art.regr - tar.regr)^2L) / sum((tar.regr - mean(tar.regr))^2L))
  rrse.perf = performance(pred.regr, measures = rrse, model = mod.regr)
  expect_equal(rrse.test, rrse$fun(pred = pred.regr))
  expect_equal(rrse.test, as.numeric(rrse.perf))
  expect_equal(sqrt((4 - 5)^2 + (11 - 10)^2 + (0 - 0)^2 + (4 - 5)^2) / sqrt((5 - 5)^2 + (10 - 5)^2 + (0 - 5)^2 + (5 - 5)^2), measureRRSE(c(5, 10, 0, 5), c(4, 11, 0, 4)))
  suppressWarnings({
    expect_equal(NA_real_, measureRRSE(0, 0))
    expect_warning(measureRRSE(0, 0))
    expect_warning(measureRRSE(c(1, 1, 1, 1), c(1, 2, 3, 4)))
  })
  expect_silent(measureRRSE(c(1, 1, 1, 0), c(2, 2, 2, 2)))
  # rae
  rae.test = sum(abs(pred.art.regr - tar.regr)) / sum(abs(tar.regr - mean(tar.regr)))
  rae.perf = performance(pred.regr, measures = rae, model = mod.regr)
  expect_equal(rae.test, rae$fun(pred = pred.regr))
  expect_equal(rae.test, as.numeric(rae.perf))
  expect_equal((abs(4 - 5) + abs(11 - 10) + abs(0 - 0) + abs(4 - 5)) / (abs(5 - 5) + abs(10 - 5) + abs(0 - 5) + abs(5 - 5)), measureRAE(c(5, 10, 0, 5), c(4, 11, 0, 4)))
  suppressWarnings({
    expect_equal(NA_real_, measureRAE(0, 0))
    expect_warning(measureRAE(0, 0))
    expect_warning(measureRAE(c(1, 1, 1, 1), c(1, 2, 3, 4)))
  })
  expect_silent(measureRAE(c(1, 1, 1, 0), c(2, 2, 2, 2)))
  # mape
  suppressWarnings({
    expect_equal(NA_real_, mape$fun(pred = pred.regr))
    expect_equal(NA_real_, measureMAPE(c(5, 10, 0, 5), c(4, 11, 0, 4)))
  })
  expect_warning(mape$fun(pred = pred.regr), regexp = "Measure is undefined if any truth value is equal to 0.")
  expect_warning(measureMAPE(c(5, 10, 0, 5), c(4, 11, 0, 4)), regexp = "Measure is undefined if any truth value is equal to 0.")
  pred.regr.mape = pred.regr
  pred.regr.mape$data$truth = c(5, 10, 1, 5) #we change the 0 target because mape is undefined
  mape.perf = performance(pred.regr.mape, measures = mape, model = mod.regr)
  mape.test = mean(c(abs((5 - 4) / 5), abs((10 - 11) / 10), abs((1 - 0) / 1), abs((5 - 4) / 5)))
  expect_equal(mape.test, mape$fun(pred = pred.regr.mape))
  expect_equal(mape.test, as.numeric(mape.perf))
  expect_equal(1 / 4 * (abs((4 - 5) / 5) + abs((11 - 10) / 10) + abs((0 - 2) / 2) + abs((4 - 5) / 5)), measureMAPE(c(5, 10, 2, 5), c(4, 11, 0, 4)))
  expect_warning(measureMAPE(0, 0))
  expect_warning(measureMAPE(c(1, 1, 1, 0), c(2, 2, 2, 2)))
  expect_silent(measureMAPE(c(1, 1, 1, 1), c(2, 2, 2, 2)))
  # msle
  msle.test = ((log(4 + 1) - log(5 + 1))^2 + (log(11 + 1) - log(10 + 1))^2 +
(log(0 + 1) - log(0 + 1))^2 + (log(4 + 1) - log(5 + 1))^2) / 4
  msle.perf = performance(pred.regr, measures = msle, model = mod.regr)
  expect_equal(msle.test, msle$fun(pred = pred.regr))
  expect_equal(msle.test, as.numeric(msle.perf))
  # msle throws error for values smaller than -1
  pred.art.regr.neg = pred.art.regr
  pred.art.regr.neg[[1L]] = -3
  expect_error(measureMSLE(tar.regr, pred.art.regr.neg),
    "values must be greater or equal -1")
  # rmsle
  rmsle.test = sqrt(msle.test)
  rmsle.perf = performance(pred.regr, measures = rmsle, model = mod.regr)
  expect_equal(rmsle.test, rmsle$fun(pred = pred.regr))
  expect_equal(rmsle.test, as.numeric(rmsle.perf))
  #tau
  tau.test = 1
  tau.perf = performance(pred.regr, measures = kendalltau, model = mod.regr)
  expect_equal(tau.test, kendalltau$fun(pred = pred.regr))
  expect_equal(tau.test, as.numeric(tau.perf))
  #rho
  rho.test = 1
  rho.perf = performance(pred.regr, measures = spearmanrho, model = mod.regr)
  expect_equal(rho.test, spearmanrho$fun(pred = pred.regr))
  expect_equal(rho.test, as.numeric(rho.perf))

  #test multiclass measures

  #mmce
  mmce.test = mean(c(1L != 1L, 2L != 0L, 0L != 0L, 1L != 2L))
  mmce.perf = performance(pred.classif, measures = mmce, model = mod.classif)
  expect_equal(mmce.test, mmce$fun(pred = pred.classif))
  expect_equal(mmce.test, as.numeric(mmce.perf))
  #acc
  acc.test = mean(c(1L != 1L, 2L != 0L, 0L != 0L, 1L != 2L))
  acc.perf = performance(pred.classif, measures = acc, model = mod.classif)
  expect_equal(acc.test, acc$fun(pred = pred.classif))
  expect_equal(acc.test, as.numeric(acc.perf))
  # colAUC binary
  colauc.tab = as.matrix(table(tar.bin, pred.art.bin)) # confusion matrix
  colauc.truepos = unname(rev(cumsum(rev(colauc.tab[2, ])))) # Number of true positives
  colauc.falsepos = unname(rev(cumsum(rev(colauc.tab[1, ])))) # Number of false positives
  colauc.totpos = sum(colauc.tab[2, ]) # The total number of positives(one number)
  colauc.totneg = sum(colauc.tab[1, ]) # The total number of negatives(one number)
  colauc.sens = colauc.truepos / colauc.totpos # Sensitivity(fraction true positives)
  colauc.omspec = colauc.falsepos / colauc.totneg # 1 − specificity(false positives)
  colauc.sens = c(colauc.sens, 0) # Numbers when we classify all as 0
  colauc.omspec = c(colauc.omspec, 0) # Numbers when we classify all as 0
  colauc.height = (colauc.sens[-1] + colauc.sens[-length(colauc.sens)]) / 2
  colauc.width = - diff(colauc.omspec) # = diff(rev(omspec))
  expect_equal(sum(colauc.height * colauc.width), colAUC(as.numeric(pred.art.bin), truth = tar.bin)[[1]])
  # colAUC with "maximum = FALSE"
  colauc.min = colAUC(c(1, 0, 1, 1), truth = tar.bin, maximum = FALSE)
  colauc.max = colAUC(c(1, 0, 1, 1), truth = tar.bin, maximum = TRUE)
  expect_equal(colauc.min[[1]], 0.25)
  expect_equal(colauc.min, 1 - colauc.max)
  # colAUC multiclass
  colauc.tab = as.matrix(table(tar.classif, pred.art.classif)) # confusion matrix
  tab = t(utils::combn(0:2, 2)) # all possible 1 vs. 1 combinations
  colauc2 = matrix(NA, 3, 1)
  for (i in 1:3) {
    cind = c(which(colnames(colauc.tab) == tab[i, 1]), which(colnames(colauc.tab) == tab[i, 2])) # column indices of i-th combination
    rind = c(which(rownames(colauc.tab) == tab[i, 1]), which(rownames(colauc.tab) == tab[i, 2])) # row indices of i-th combination
    colauc.tab.part = colauc.tab[rind, cind] # resulting patrial matrix
    colauc.truepos = unname(rev(cumsum(rev(colauc.tab.part[2, ])))) # Number of true positives
    colauc.falsepos = unname(rev(cumsum(rev(colauc.tab.part[1, ])))) # Number of false positives
    colauc.totpos = sum(colauc.tab.part[2, ]) # The total number of positives(one number)
    colauc.totneg = sum(colauc.tab.part[1, ]) # The total number of negatives(one number)
    if (colauc.totpos > 0) {
      colauc.sens = colauc.truepos / colauc.totpos # Sensitivity(fraction true positives)
    } else {
      colauc.sens = c(1, 1)
    }
    if (colauc.totneg > 0) {
      colauc.omspec = colauc.falsepos / colauc.totneg # 1 − specificity(false positives)
    } else {
      colauc.omspec = c(1, 1)
    }
    colauc.sens = c(colauc.sens, 0) # Numbers when we classify all as 0
    colauc.omspec = c(colauc.omspec, 0) # Numbers when we classify all as 0
    colauc.height = (colauc.sens[-1] + colauc.sens[-length(colauc.sens)]) / 2
    colauc.width = -diff(colauc.omspec) # = diff(rev(colauc.omspec))
  if (sum(colauc.height * colauc.width) < 0.5) {
    colauc2[i, 1] = 1 - sum(colauc.height * colauc.width)  # calculate AUC using formula for the area of a trapezoid
  } else {
    colauc2[i, 1] = sum(colauc.height * colauc.width)  # calculate AUC using formula for the area of a trapezoid
  }
}
  expect_equal(colauc2[, 1], as.numeric(colAUC(as.numeric(pred.art.classif), truth = tar.classif)[, 1]))
  # multiclass.auc
  expect_equal(as.numeric(performance(pred.bin, measures = list(multiclass.aunu,
    multiclass.aunp, multiclass.au1u, multiclass.au1p))),
    as.numeric(rep(performance(pred.bin, measures = auc), 4)))
  auc.lrn = makeLearner("classif.rpart", predict.type = "prob")
  auc.fit = train(auc.lrn, iris.task)
  auc.pred.constant = predict(auc.fit, subsetTask(iris.task, 1:50))
  suppressWarnings({
    expect_equal(c(multiclass.aunu = NA_real_, multiclass.aunp = NA_real_), performance(auc.pred.constant, list(multiclass.aunu, multiclass.aunp)))
    expect_warning(measureAUNU(getPredictionProbabilities(auc.pred.constant, auc.pred.constant$task.desc$class.levels), auc.pred.constant$data$truth))
    expect_warning(measureAUNP(getPredictionProbabilities(auc.pred.constant, auc.pred.constant$task.desc$class.levels), auc.pred.constant$data$truth))
  })

  p1 = p2 = matrix(c(0.1, 0.9, 0.2, 0.8), 2, 2, byrow = TRUE)
  colnames(p1) = c("a", "b")
  colnames(p2) = c("b", "a")
  y1 = factor(c("a", "b"))
  y2 = factor(c("b", "b"))
  # multiclass.brier
  expect_equal(measureMulticlassBrier(p1, y1), 0.5 * ((1 - 0.1)^2 + (0 - 0.9)^2 + (0 - 0.2)^2 + (1 - 0.8)^2))
  expect_equal(measureMulticlassBrier(p1, y2), 0.5 * ((0 - 0.1)^2 + (1 - 0.9)^2 + (0 - 0.2)^2 + (1 - 0.8)^2))
  expect_equal(measureMulticlassBrier(p2, y1), 0.5 * ((1 - 0.9)^2 + (0 - 0.1)^2 + (1 - 0.2)^2 + (0 - 0.8)^2))
  # logloss
  expect_equal(measureLogloss(p1, y1), -mean(log(c(0.1, 0.8))))
  expect_equal(measureLogloss(p1, y2), -mean(log(c(0.9, 0.8))))
  expect_equal(measureLogloss(p2, y1), -mean(log(c(0.9, 0.2))))

  pred.probs = getPredictionProbabilities(pred.classif)
  pred.probs[pred.probs > 1 - 1e-15] = 1 - 1e-15
  pred.probs[pred.probs < 1e-15] = 1e-15
  logloss.test = -1 * mean(log(pred.probs[model.matrix(~ . + 0, data = as.data.frame(tar.classif)) - pred.probs > 0]))
  logloss.perf = performance(pred.classif, measures = logloss, model = mod.classif)
  expect_equal(logloss.test, logloss$fun(pred = pred.classif))
  expect_equal(logloss.test, as.numeric(logloss.perf))

  #ssr
  pred.probs = getPredictionProbabilities(pred.classif)
  ssr.test = mean(vnapply(seq_row(pred.probs), function(i) {pred.probs[i, tar.classif[i]]}) / sqrt(rowSums(pred.probs^2)))
  ssr.perf = performance(pred.classif, measures = ssr, model = mod.classif)
  expect_equal(ssr.test, ssr$fun(pred = pred.classif))
  expect_equal(ssr.test, as.numeric(ssr.perf))
  expect_equal(measureSSR(p1, y1), 0.5 * (0.1 / sqrt(0.1^2 + 0.9^2) + 0.8 / sqrt(0.2^2 + 0.8^2)))
  expect_equal(measureSSR(p1, y2), 0.5 * (0.9 / sqrt(0.1^2 + 0.9^2) + 0.8 / sqrt(0.2^2 + 0.8^2)))
  expect_equal(measureSSR(p2, y1), 0.5 * (0.9 / sqrt(0.1^2 + 0.9^2) + 0.2 / sqrt(0.2^2 + 0.8^2)))
  expect_equal(measureSSR(p2[1, , drop = FALSE], y2[1]), 0.1 / sqrt(0.1^2 + 0.9^2))
  expect_equal(measureSSR(p2[1, , drop = FALSE], y1[1]), 0.9 / sqrt(0.1^2 + 0.9^2))
  #qsr
  qsr.test = 1 - mean(rowSums((pred.probs - model.matrix(~ . + 0, data = as.data.frame(tar.classif)))^2))
  qsr.perf = performance(pred.classif, measures = qsr, model = mod.classif)
  expect_equal(qsr.test, qsr$fun(pred = pred.classif))
  expect_equal(qsr.test, as.numeric(qsr.perf))
  expect_equal(measureQSR(p1, y1), 1 - 0.5 * ((1 - 0.1)^2 + (0 - 0.9)^2 + (0 - 0.2)^2 + (1 - 0.8)^2))
  expect_equal(measureQSR(p1, y2), 1 - 0.5 * ((0 - 0.1)^2 + (1 - 0.9)^2 + (0 - 0.2)^2 + (1 - 0.8)^2))
  expect_equal(measureQSR(p2, y1), 1 - 0.5 * ((1 - 0.9)^2 + (0 - 0.1)^2 + (1 - 0.2)^2 + (0 - 0.8)^2))
  expect_equal(measureQSR(p2[1, , drop = FALSE], y2[1]), 1 - (1 - 0.1)^2 - (0 - 0.9)^2)
  expect_equal(measureQSR(p2[1, , drop = FALSE], y1[1]), 1 - (1 - 0.9)^2 - (0 - 0.1)^2)
  #lsr
  lsr.test = mean(log(pred.probs[model.matrix(~ . + 0, data = as.data.frame(tar.classif)) - pred.probs > 0]))
  lsr.perf = performance(pred.classif, measures = lsr, model = mod.classif)
  expect_equal(lsr.test, lsr$fun(pred = pred.classif))
  expect_equal(lsr.test, as.numeric(lsr.perf))
  expect_equal(measureLSR(p1, y1), mean(log(c(0.1, 0.8))))
  expect_equal(measureLSR(p1, y2), mean(log(c(0.9, 0.8))))
  expect_equal(measureLSR(p2, y1), mean(log(c(0.9, 0.2))))
  expect_equal(measureLSR(p2[1, , drop = FALSE], y2[1]), log(0.1))
  expect_equal(measureLSR(p2[1, , drop = FALSE], y1[1]), log(0.9))
  #kappa
  p0 = 0.5
  pe = (0.25 * 0.25 + 0.5 * 0.5 + 0.25 * 0.25) / 1
  kappa.test = 1 - (1 - p0) / (1 - pe)
  kappa.perf = performance(pred.classif, measures = kappa, model = mod.classif)
  expect_equal(measureKAPPA(tar.classif, pred.art.classif), kappa.test)
  expect_equal(measureKAPPA(tar.classif, pred.art.classif), as.numeric(kappa.perf))
  #wkappa
  conf.mat = matrix(c(1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L), nrow = 3L) / 4L
  expected.mat = c(0.25, 0.5, 0.25) %*% t(c(0.25, 0.5, 0.25))
  weights = matrix(c(0, 1, 4, 1, 0, 1, 4, 1, 0), nrow = 3L)
  wkappa.test = 1 - sum(weights * conf.mat) / sum(weights * expected.mat)
  wkappa.perf = performance(pred.classif, measures = wkappa, model = mod.classif)
  expect_equal(measureWKAPPA(tar.classif, pred.art.classif), wkappa.test)
  expect_equal(measureWKAPPA(tar.classif, pred.art.classif), as.numeric(wkappa.perf))
  tar.classif2 = tar.classif
  pred.art.classif2 = pred.art.classif
  levels(tar.classif2) = as.numeric(levels(tar.classif))^2
  levels(pred.art.classif2) = as.numeric(levels(pred.art.classif))^2
  expect_equal(measureWKAPPA(tar.classif2, pred.art.classif2), wkappa.test)

  #test binaryclass measures

  #brier
  pred.probs = getPredictionProbabilities(pred.bin)
  brier.test = mean((as.numeric(tar.bin == "0") - pred.probs)^2)
  brier.perf = performance(pred.bin, measures = brier, model = mod.bin)
  expect_equal(brier.test, brier$fun(pred = pred.bin))
  expect_equal(brier.test, as.numeric(brier.perf))
  expect_equal(measureBrier(c(1, 1, 0), c("a", "a", "a"), "b", "a"), 1 / 3)
  expect_equal(measureBrier(c(1, 1, 1), c("a", "a", "a"), "b", "a"), 0)
  expect_equal(measureBrier(c(0, 0, 0), c("a", "a", "a"), "b", "a"), 1)
  #brier.scaled
  inc = mean(pred.probs)
  brier.test.max = inc * (1 - inc)^2 + (1 - inc) * inc^2
  brier.scaled.test = 1 - brier.test / brier.test.max
  brier.scaled.perf = performance(pred.bin, measures = brier.scaled, model = mod.bin)
  expect_equal(brier.scaled.test, brier.scaled$fun(pred = pred.bin))
  expect_equal(brier.scaled.test, as.numeric(brier.scaled.perf))
  expect_equal(measureBrierScaled(c(1, 1, 0), c("a", "a", "a"), "b", "a"), 1 - ((1 / 3) / (2 / 3 * 1 / 3)))
  expect_equal(measureBrierScaled(c(1, 1, 1), c("a", "a", "a"), "b", "a"), 1 - ((0) / (1 * 0)))
  expect_equal(measureBrierScaled(c(0, 0, 0), c("a", "a", "a"), "b", "a"), 1 - ((1) / (0 * 1)))
  #tp
  tp.test = sum(tar.bin == pred.art.bin & pred.art.bin == 0L)
  tp.perf = performance(pred.bin, measures = tp, model = mod.bin)
  expect_equal(tp.test, tp$fun(pred = pred.bin))
  expect_equal(tp.test, as.numeric(tp.perf))
  #tn
  tn.test = sum(tar.bin == pred.art.bin & pred.art.bin == 1L)
  tn.perf = performance(pred.bin, measures = tn, model = mod.bin)
  expect_equal(tn.test, tn$fun(pred = pred.bin))
  expect_equal(tn.test, as.numeric(tn.perf))
  #fp
  fp.test = sum(tar.bin != pred.art.bin & pred.art.bin == 0L)
  fp.perf = performance(pred.bin, measures = fp, model = mod.bin)
  expect_equal(fp.test, fp$fun(pred = pred.bin))
  expect_equal(fp.test, as.numeric(fp.perf))
  #fn
  fn.test = sum(tar.bin != pred.art.bin & pred.art.bin == 1L)
  fn.perf = performance(pred.bin, measures = fn, model = mod.bin)
  expect_equal(fn.test, fn$fun(pred = pred.bin))
  expect_equal(fn.test, as.numeric(fn.perf))
  #tpr
  tpr.test = tp.test / sum(tar.bin == 0L)
  tpr.perf = performance(pred.bin, measures = tpr, model = mod.bin)
  expect_equal(tpr.test, tpr$fun(pred = pred.bin))
  expect_equal(tpr.test, as.numeric(tpr.perf))
  #tnr
  tnr.test = tn.test / sum(tar.bin == 1L)
  tnr.perf = performance(pred.bin, measures = tnr, model = mod.bin)
  expect_equal(tnr.test, tnr$fun(pred = pred.bin))
  expect_equal(tnr.test, as.numeric(tnr.perf))
  #fpr
  fpr.test = fp.test / sum(tar.bin != 0L)
  fpr.perf = performance(pred.bin, measures = fpr, model = mod.bin)
  expect_equal(fpr.test, fpr$fun(pred = pred.bin))
  expect_equal(fpr.test, as.numeric(fpr.perf))
  #fnr
  fnr.test = fn.test / sum(tar.bin != 1L)
  fnr.perf = performance(pred.bin, measures = fnr, model = mod.bin)
  expect_equal(fnr.test, fnr$fun(pred = pred.bin))
  expect_equal(fnr.test, as.numeric(fnr.perf))
  #ppv
  ppv.test = tp.test / sum(pred.art.bin == 0L)
  ppv.perf = performance(pred.bin, measures = ppv, model = mod.bin)
  expect_equal(ppv.test, ppv$fun(pred = pred.bin))
  expect_equal(ppv.test, as.numeric(ppv.perf))
  #npv
  npv.test = tn.test / sum(pred.art.bin == 1L)
  npv.perf = performance(pred.bin, measures = npv, model = mod.bin)
  expect_equal(npv.test, npv$fun(pred = pred.bin))
  expect_equal(npv.test, as.numeric(npv.perf))
  #fdr
  fdr.test = fp.test / sum(pred.art.bin == 0L)
  fdr.perf = performance(pred.bin, measures = fdr, model = mod.bin)
  expect_equal(fdr.test, fdr$fun(pred = pred.bin))
  expect_equal(fdr.test, as.numeric(fdr.perf))
  #bac
  bac.test = 0.5 * (tpr.test / (tpr.test + fnr.test) + tnr.test /
(tnr.test + fpr.test))
  bac.perf = performance(pred.bin, measures = bac, model = mod.bin)
  expect_equal(bac.test, bac$fun(pred = pred.bin))
  expect_equal(bac.test, as.numeric(bac.perf))
  #ber
  ber.test = 1L - bac.test
  ber.perf = performance(pred.bin, measures = ber, model = mod.bin)
  expect_equal(ber.test, ber$fun(pred = pred.bin))
  expect_equal(ber.test, as.numeric(ber.perf))
  #auc
  auc.test = (tpr.test + tnr.test) / 2L
  auc.perf = performance(pred.bin, measures = auc, model = mod.bin)
  expect_equal(auc.test, auc$fun(pred = pred.bin))
  expect_equal(auc.test, as.numeric(auc.perf))
  #mcc
  mcc.test = (tp.test * tn.test - fp.test * fn.test) /
    sqrt((tp.test + fp.test) * (tp.test + fn.test) *
(tn.test + fp.test) * (tn.test + fn.test))
   mcc.perf = performance(pred.bin, measures = mcc, model = mod.bin)
  expect_equal(mcc.test, mcc$fun(pred = pred.bin))
  expect_equal(mcc.test, as.numeric(mcc.perf))
  #f1
  f1.test = 2 * tp.test / (sum(tar.bin == 0L) + sum(pred.art.bin == 0L))
  f1.perf = performance(pred.bin, measures = f1, model = mod.bin)
  expect_equal(f1.test, f1$fun(pred = pred.bin))
  expect_equal(f1.test, as.numeric(f1.perf))
  #gmean
  gmean.test = sqrt((tp.test / (tp.test + fn.test)) * tn.test / (tn.test + fp.test))
  gmean.perf = performance(pred.bin, measures = gmean, model = mod.bin)
  expect_equal(gmean.test, gmean$fun(pred = pred.bin))
  expect_equal(gmean.test, as.numeric(gmean.perf))
  #gpr
  gpr.test = sqrt(ppv.test * tpr.test)
  gpr.perf = performance(pred.bin, measures = gpr, model = mod.bin)
  expect_equal(gpr.test, gpr$fun(pred = pred.bin))
  expect_equal(gpr.test, as.numeric(gpr.perf))

  #test multilabel measures

  # create response and predictions using all possible combinations
  #bincombo = matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE), ncol = 2, byrow = TRUE)
  #multi.y = bincombo[rep(1:4, times = 4),]
  #multi.p = bincombo[rep(1:4, each = 4),]

  multi.y = getPredictionTruth(pred.multilabel)
  multi.p = getPredictionResponse(pred.multilabel)
  expect_equal(unname(multi.y), unname(as.matrix(getTaskTargets(task.multilabel))))
  expect_equal(pred.art.multilabel, unname(multi.p))

  # create true-false and true-true vector used for manual computation of measures
  tf = c(TRUE, FALSE)
  tt = c(TRUE, TRUE)

  # this is copy-paste from the mldr::mldr_evaluate function which is needed to compare with the mldr measures
  counters = data.frame(
    RealPositives = rowSums(multi.y),
    RealNegatives = rowSums(!multi.y),
    PredictedPositives = rowSums(multi.p),
    PredictedNegatives = rowSums(!multi.p),
    TruePositives = rowSums(multi.y & multi.p),
    TrueNegatives = rowSums(!multi.y & !multi.p))

  #hamloss: how many values are not identical
  hamloss.test = mean(as.vector(multi.y) != as.vector(multi.p))
  hamloss.perf = performance(pred.multilabel, measures = multilabel.hamloss, model = mod.multilabel)
  expect_equal(hamloss.test, multilabel.hamloss$fun(pred = pred.multilabel))
  expect_equal(hamloss.test, as.numeric(hamloss.perf))
  # check best and worst value
  expect_equal(measureMultilabelHamloss(multi.y, multi.y), multilabel.hamloss$best)
  expect_equal(measureMultilabelHamloss(multi.y, !multi.y), multilabel.hamloss$worst)
  # compare with mldr
  expect_equal(mldr:::mldr_HL(multi.y, multi.p), measureMultilabelHamloss(multi.y, multi.p))
  # mldr defines the accuracy as 1-hamloss
  expect_equal(mldr:::mldr_Accuracy(counters), 1 - measureMultilabelHamloss(multi.y, multi.p))
  # manual checks
  expect_equal(measureMultilabelHamloss(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 1 / 2) # 1 of 2 values are wrong
  expect_equal(measureMultilabelHamloss(cbind(tf, tf), cbind(tf, tt)), 1 / 4) # 1 of 4 values are wrong

  #subset01: how many rows are not identical
  subset01.test = mean(rowSums(multi.y == multi.p) != ncol(multi.y))
  subset01.perf = performance(pred.multilabel, measures = multilabel.subset01, model = mod.multilabel)
  expect_equal(subset01.test, multilabel.subset01$fun(pred = pred.multilabel))
  expect_equal(subset01.test, as.numeric(subset01.perf))
  # check best and worst
  expect_equal(measureMultilabelSubset01(multi.y, multi.y), multilabel.subset01$best)
  expect_equal(measureMultilabelSubset01(multi.y, !multi.y), multilabel.subset01$worst)
  # compare with mldr: we have implemented the subset loss which is 1 - subset accuracy
  expect_equal(mldr:::mldr_SubsetAccuracy(multi.y, multi.p), 1 - measureMultilabelSubset01(multi.y, multi.p))
  # manual checks
  expect_equal(measureMultilabelSubset01(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 1) # 1 of 1 obs is wrong
  expect_equal(measureMultilabelSubset01(cbind(tf, tf), cbind(tf, tt)), 1 / 2) # 1 of 2 obs is wrong

  #f1mult
  f1.test = vnapply(seq_row(multi.y), function(i) 2 * sum(multi.y[i, ] * multi.p[i, ]) / (sum(multi.y[i, ]) + sum(multi.p[i, ])))
  f1.test[is.na(f1.test)] = 1
  f1.test = mean(f1.test)
  f1.perf = performance(pred.multilabel, measures = multilabel.f1, model = mod.multilabel)
  expect_equal(f1.test, multilabel.f1$fun(pred = pred.multilabel))
  expect_equal(f1.test, as.numeric(f1.perf))
  # check best and worst
  expect_equal(measureMultiLabelF1(multi.y, multi.y), multilabel.f1$best)
  expect_equal(measureMultiLabelF1(multi.y, !multi.y), multilabel.f1$worst)
  # compare with mldr: mldr has a bug when RealPositives or PredictedPositives are 0 (see https://github.com/fcharte/mldr/issues/36)
  expect_equal(mldr:::mldr_FMeasure(counters[-3, ]), measureMultiLabelF1(multi.y[-3, ], multi.p[-3, ]))
  # manual checks
  expect_equal(measureMultiLabelF1(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 2 * 1 / 3) # 1 TRUE-TRUE match of 3 TRUE values
  expect_equal(measureMultiLabelF1(rbind(tf, tf), rbind(tf, tt)), mean(c(2 * 1 / 2, 2 * 1 / 3))) # 1 TRUE-TRUE match of 2 and 3 TRUE values per obs

  #accmult
  acc.test = vnapply(seq_row(multi.y), function(i) sum(multi.y[i, ] & multi.p[i, ]) / (sum(multi.y[i, ] | multi.p[i, ])))
  acc.test[is.na(acc.test)] = 1
  acc.test = mean(acc.test)
  acc.perf = performance(pred.multilabel, measures = multilabel.acc, model = mod.multilabel)
  expect_equal(acc.test, multilabel.acc$fun(pred = pred.multilabel))
  expect_equal(acc.test, as.numeric(acc.perf))
  # check best and worst
  expect_equal(measureMultilabelACC(multi.y, multi.y), multilabel.acc$best)
  expect_equal(measureMultilabelACC(multi.y, !multi.y), multilabel.acc$worst)
  # compare with mldr: jaccard index is not implemented in mldr see https://github.com/fcharte/mldr/issues/28
  # manual checks
  expect_equal(measureMultilabelACC(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 1 / 2)
  expect_equal(measureMultilabelACC(rbind(tf, tf), rbind(tf, tt)), mean(c(1, 1 / 2)))

  #ppvmult
  ppv.test = vnapply(seq_row(multi.y), function(i) sum(multi.y[i, ] & multi.p[i, ]) / (sum(multi.p[i, ])))
  ppv.test = mean(ppv.test, na.rm = TRUE)
  ppv.perf = performance(pred.multilabel, measures = multilabel.ppv, model = mod.multilabel)
  expect_equal(ppv.test, multilabel.ppv$fun(pred = pred.multilabel))
  expect_equal(ppv.test, as.numeric(ppv.perf))
  # check best and worst
  expect_equal(measureMultilabelPPV(multi.y, multi.y), multilabel.ppv$best)
  expect_equal(measureMultilabelPPV(multi.y, !multi.y), multilabel.ppv$worst)
  # compare with mldr
  expect_equal(mldr:::mldr_Precision(counters), measureMultilabelPPV(multi.y, multi.p))
  # manual checks
  expect_equal(measureMultilabelPPV(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 1 / 2)
  expect_equal(measureMultilabelPPV(rbind(tf, tf), rbind(tf, tt)), mean(c(1 / 1, 1 / 2)))

  #tprmult
  tpr.test = vnapply(seq_row(multi.y), function(i) sum(multi.y[i, ] & multi.p[i, ]) / (sum(multi.y[i, ])))
  tpr.test = mean(tpr.test, na.rm = TRUE)
  tpr.perf = performance(pred.multilabel, measures = multilabel.tpr, model = mod.multilabel)
  expect_equal(tpr.test, multilabel.tpr$fun(pred = pred.multilabel))
  expect_equal(tpr.test, as.numeric(tpr.perf))
  # check best and worst
  expect_equal(measureMultilabelTPR(multi.y, multi.y), multilabel.tpr$best)
  expect_equal(measureMultilabelTPR(multi.y, !multi.y), multilabel.tpr$worst)
  # compare with mldr
  expect_equal(mldr:::mldr_Recall(counters), measureMultilabelTPR(multi.y, multi.p))
  # manual checks
  expect_equal(measureMultilabelTPR(matrix(tf, ncol = 2), matrix(tt, ncol = 2)), 1 / 1)
  expect_equal(measureMultilabelTPR(rbind(tf, tf), rbind(tf, tt)), mean(c(1 / 1, 1 / 1)))

  #test survival measures

  #cindex
  pos = pred.surv$data[pred.surv$data$truth.event == TRUE, "response"]
  neg = pred.surv$data[pred.surv$data$truth.event == FALSE, "response"]
  cons = c(ifelse(pos[1L] > neg, 1L, 0L), ifelse(pos[2L] > neg, 1L, 0L))
  ties = c(ifelse(pos[1L] == neg, 0.5, 0), ifelse(pos[2L] == neg, 0.5, 0))
  n.pairs = length(pos) * length(neg)
  cindex.test = sum(c(cons, ties)) / n.pairs
  cindex.perf = performance(pred.surv, measures = cindex, model = mod.surv)
  expect_equal(cindex.test, cindex$fun(pred = pred.surv))
  expect_equal(cindex.test, as.numeric(cindex.perf))

  #test cost-sensitive measures

  #meancosts
  meancosts.test = (0 + 0 + 0 + 1) / 4L
  meancosts.perf = performance(pred.costsens, measures = meancosts,
   model = mod.costsens, task = task.costsens)
  expect_equal(meancosts.test,
    meancosts$fun(pred = pred.costsens, task = task.costsens))
  expect_equal(meancosts.test, as.numeric(meancosts.perf))
  #mcp
  mcp.test = meancosts.test - 0
  mcp.perf = performance(pred.costsens, measures = mcp,
    task = task.costsens, model = mod.costsens)
  expect_equal(mcp.test, mcp$fun(pred = pred.costsens, task = task.costsens))
  expect_equal(mcp.test, as.numeric(mcp.perf))

  #test clustering

  #db
  c2 = c(3, 1)
  c1 = c((1 + 2 + 4) / 3, (3 + 4 + 2) / 3)
  s1 = sqrt((sum((data.cluster[1, ] - c1)^2) + sum((data.cluster[2, ] - c1)^2) +
    sum((data.cluster[4, ] - c1)^2)) / 3L)
  M = sqrt(sum((c2 - c1)^2))
  db.test = s1 / M
  db.perf = performance(pred.cluster, measures = db,
    model = mod.cluster, feats = data.cluster)
  expect_equal(db.test, db$fun(task = task.cluster,
   pred = pred.cluster, feats = data.cluster))
  expect_equal(db.test, as.numeric(db.perf))

  #dunn
  exdist = min(sqrt(sum((c(1, 3) - c(3, 1))^2)), sqrt(sum((c(2, 4) - c(3, 1))^2)),
    sqrt(sum((c(4, 3) - c(3, 2))^2)))
  indist = max(sqrt(sum((c(1, 3) - c(2, 4))^2)), sqrt(sum((c(1, 3) - c(4, 2))^2)),
    sqrt(sum((c(2, 4) - c(4, 2))^2)))
  dunn.test = exdist / indist
  dunn.perf = performance(pred.cluster, measures = dunn,
    model = mod.cluster, feats = data.cluster)
  expect_equal(dunn.test,
    dunn$fun(pred = pred.cluster, feats = data.cluster))
  expect_equal(dunn.test, as.numeric(dunn.perf))
  #g1 index
  exsum = sqrt(sum((c(1, 3) - c(3, 1))^2)) + sqrt(sum((c(2, 4) - c(3, 1))^2)) +
    sqrt(sum((c(4, 3) - c(3, 2))^2))
  insum = sqrt(sum((c(1, 3) - c(2, 4))^2)) + sqrt(sum((c(1, 3) - c(4, 2))^2)) +
    sqrt(sum((c(2, 4) - c(4, 2))^2))
  g1.test = exsum / insum
  g1.perf = performance(pred.cluster, measures = G1,
    model = mod.cluster, feats = data.cluster)
  expect_equal(g1.test, G1$fun(pred = pred.cluster, feats = data.cluster))
  expect_equal(g1.test, as.numeric(g1.perf))
  #g2 index
  dists = as.matrix(dist(data.cluster, method = "euclidian"))
  c2.dists = as.vector(dists[, 3L])
  c2.dists = c2.dists[c2.dists != 0L]
  c1.dists = unique(as.vector(dists [-3L, -3L]))
  c1.dists = c1.dists[c1.dists != 0L]
  con.pairs = vapply(c1.dists, function(x) x < c2.dists,
   logical(length = length(c2.dists)))
  con.pairs = sum(rowSums(con.pairs))
  dis.pairs = vapply(c2.dists, function(x) x < c1.dists,
   logical(length = length(c1.dists)))
  dis.pairs = sum(rowSums(dis.pairs))
  g2.test = (con.pairs - dis.pairs) / (con.pairs + dis.pairs)
  g2.perf = performance(pred.cluster, measures = G2,
    model = mod.cluster, feats = data.cluster)
  expect_equal(g2.test, G2$fun(pred = pred.cluster, feats = data.cluster))
  expect_equal(g2.test, as.numeric(g2.perf))
  #silhouette
  dists = as.matrix(clusterSim::dist.GDM(data.cluster))
  ais = replace(dists, dists == 0, NA)[-3L, -3L]
  ais = apply(ais, MARGIN = 2L, mean, na.rm = TRUE)
  bis = dists[-3L, 3L]
  sil.data = data.frame(t(rbind(ais, bis)))
  sils = (sil.data$bis - sil.data$ais) / pmax(sil.data$bis, sil.data$ais)
  silhouette.test = sum(sils) / nrow(data.cluster)
  silhouette.perf = performance(pred.cluster, measures = silhouette,
    model = mod.cluster, feats = data.cluster)
  expect_equal(silhouette.test, silhouette$fun(pred = pred.cluster, feats = data.cluster))
  expect_equal(object = silhouette.test, as.numeric(silhouette.perf))

  #test that some measures are only transformations of each other

  #qsr is identical to the 1 - multiclass brier
  expect_equal(1 - measureMulticlassBrier(p1, y1), measureQSR(p1, y1), check.names = FALSE)
  qsr.bin.perf = performance(pred.bin, measures = qsr, model = mod.bin)
  expect_equal(1 - 2 * brier.perf, qsr.bin.perf, check.names = FALSE)

  expect_equal(lsr.perf, -1 * logloss.perf, check.names = FALSE)

  #multiclass brier for a two class problem should be two times the binary brier score.
  multiclass.brier.twoclass.perf = performance(pred.bin, measures = multiclass.brier, model = mod.bin)
  expect_equal(2 * brier.perf, multiclass.brier.twoclass.perf, check.names = FALSE)

})

test_that("getDefaultMeasure", {
  expect_equal(mmce, getDefaultMeasure(iris.task))
  expect_equal(mmce, getDefaultMeasure(getTaskDesc(iris.task)))
  expect_equal(mmce, getDefaultMeasure(makeLearner("classif.rpart")))
  expect_equal(mmce, getDefaultMeasure("classif.rpart"))
  expect_equal(mmce, getDefaultMeasure("classif"))
})

test_that("measure properties", {
  #hasMeasureProps yields correct properties
  expect_true(all(vlapply(listMeasures(create = TRUE),
    function(m) {
      res = hasMeasureProperties(m, m$properties)
      all(res) & length(res) > 0
      })))
  props = listMeasureProperties()
  #all props exist in mlr$measure.properties
  expect_true(all(vlapply(listMeasures(create = TRUE),
    function(m) {
      res = all(getMeasureProperties(m) %in% props)
      all(res) & length(res) > 0
    })))
})

test_that("measures quickcheck", {
  skip_on_cran()
  options(warn = 2)
  ms = list(mmce, acc, bac, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1)
  lrn = makeLearner("classif.rpart")

  quickcheckTest(
    quickcheck::forall(data = as.data.frame(quickcheck::rmatrix(elements = quickcheck::rinteger, nrow = c(min = 2, max = 10000), ncol = c(min = 1, max = 100))),
      {
        classes = factor(c("foo", "bar"))
        data$target = rep_len(classes, length.out = nrow(data))

        train.ids = 1:(2 * nrow(data) / 3)
        test.ids = setdiff(seq_len(nrow(data)), train.ids)
        task = makeClassifTask(data = data, target = "target")

        mod = train(lrn, task = task, subset = train.ids)
        pred = predict(mod, task = task, subset = test.ids)
        perf = performance(pred, measures = ms)

        is.numeric(unlist(perf)) && all(perf >= 0 && perf <= 1)
      }
    ),
    about = "binary classification measures",
    sample.size = 100
  )
})

test_that("measures ppv denominator 0", {
  set.seed(1)
  task = sonar.task
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  r = holdout(lrn, task)
  d = generateThreshVsPerfData(r, measures = list(tpr, ppv), gridsize = 5)
  expect_equal(length(which(is.na(d$data))), 0)
  lrns = list(makeLearner("classif.randomForest", predict.type = "prob"), makeLearner("classif.rpart", predict.type = "prob"))
  tasks = list(bc.task, sonar.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, ber)
  bmrk = benchmark(lrns, tasks, rdesc, measures = meas)
  pr = generateThreshVsPerfData(bmrk, measures = list(tpr, ppv))
  expect_equal(length(which(is.na(pr$data))), 0)
})

test_that("measures MCC denominator 0 (#1736)", {
  res = measureMCC(c(TRUE, TRUE, TRUE), c(TRUE, TRUE, TRUE), TRUE, FALSE)
  expect_equal(res, 0)
})
