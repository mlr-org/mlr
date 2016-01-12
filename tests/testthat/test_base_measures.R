context("measures")

test_that("measures", {
  ct = binaryclass.task
  options(warn = 2)
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
  pred.multilabel$data[,4:5] = pred.art.multilabel
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
  pred.surv$data[,"response"] = pred.art.surv
  #for costsensitive
  tar.costsens = factor(c("a", "b", "c", "a"))
  pred.art.costsens = factor(c("a", "b", "c", "c"))
  data.costsens = data.frame(var1, var2)
  costs = matrix(c(0, 1, 2, 1, 0, 2, 1, 2, 0, 0, 2,1), nrow = 4L, byrow = TRUE)
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
  names(sse.perf) = NULL
  expect_equal(sse.test, sse.perf, sse$fun(pred = pred.regr))
  #mse
  mse.test = mean(sq.errs)
  mse.perf = performance(pred.regr, measures = mse, model = mod.regr)
  names(mse.perf) = NULL
  expect_equal(mse.test, mse.perf, mse$fun(pred = pred.regr))
  #rmse
  rmse.test = sqrt(mse.test)
  rmse.perf = performance(pred.regr, measures = rmse, model = mod.regr)
  names(rmse.perf) = NULL
  expect_equal(rmse.test, rmse.perf, rmse$fun(pred = pred.regr))
  #medse
  medse.test = median(sq.errs)
  medse.perf = performance(pred.regr, measures = medse, model = mod.regr)
  names(medse.perf) = NULL
  expect_equal(medse.test, medse.perf, medse$fun(pred = pred.regr))
  #sae
  abs.errs = abs(c(5 - 4, 10 - 11, 0 - 0, 5 - 4))
  sae.test = sum(abs.errs)
  sae.perf = performance(pred.regr, measures = sae, model = mod.regr)
  names(sae.perf) = NULL
  expect_equal(sae.test, sae.test, sae$fun(pred = pred.regr))
  #mae
  mae.test = mean(abs.errs)
  mae.perf = performance(pred.regr, measures = mae, model = mod.regr)
  names(mae.perf) = NULL
  expect_equal(mae.test, mae.perf, mae$fun(pred = pred.regr))
  #medae
  medae.test = median(abs.errs)
  medae.perf = performance(pred.regr, measures = medae, model = mod.regr)
  names(medae.perf) = NULL
  expect_equal(medae.test, medae.perf, medae$fun(pred = pred.regr))


  #test multiclass measures

  #mmce
  mmce.test = mean(c(1L != 1L, 2L != 0L, 0L != 0L, 1L != 2L))
  mmce.perf = performance(pred.classif, measures = mmce, model = mod.classif)
  names(mmce.perf) = NULL
  expect_equal(mmce.test, mmce.perf, mmce$fun(pred = pred.classif))
  #acc
  acc.test = mean(c(1L != 1L, 2L != 0L, 0L != 0L, 1L != 2L))
  acc.perf = performance(pred.classif, measures = acc, model = mod.classif)
  names(acc.perf) = NULL
  expect_equal(acc.test, acc.perf, acc$fun(pred = pred.classif))
  #multiclass.auc
  n.cl = length(levels(tar.classif))
  pred.probs = getPredictionProbabilities(pred.classif)
  predictor = vnapply(1:length(pred.art.classif), function(i) {
    pred.probs[i, pred.art.classif[i]]
  })
  names(predictor) = pred.art.classif
  level.grid = t(combn(as.numeric(levels(tar.classif)), m = 2L))
  level.grid = rbind(level.grid, level.grid[, ncol(level.grid):1])
  aucs = numeric(nrow(level.grid))
  for(i in 1:nrow(level.grid)){
    ranks = sort(rank(predictor[names(predictor) %in% level.grid[i, ]]))
    ranks = ranks[names(ranks) == level.grid[i]]
    n = length(ranks)
    ranks.sum = sum(ranks)
    aucs[i] = ranks.sum - n * (n + 1) / 2
  }
  multiclass.auc.test = 1 / (n.cl * (n.cl - 1)) * sum(aucs)
  multiclass.auc.perf = performance(pred.classif,
   measures = multiclass.auc, model = mod.classif)
  names(multiclass.auc.perf) = NULL
  expect_equal(multiclass.auc.test, multiclass.auc.perf,
   multiclass.auc$fun(pred = pred.classif))

  #test binaryclass measures

  #brier
  pred.probs = getPredictionProbabilities(pred.bin)
  brier.test = mean((as.numeric(tar.bin == "0") - pred.probs)^2)
  brier.perf = performance(pred.bin, measures = brier, model = mod.bin)
  names(brier.perf) = NULL
  expect_equal(brier.test, brier.perf, brier$fun(pred = pred.bin))
  #tp
  tp.test = sum(tar.bin == pred.art.bin & pred.art.bin == 0L)
  tp.perf = performance(pred.bin, measures = tp, model = mod.bin)
  names(tp.perf) = NULL
  expect_equal(tp.test, tp.perf, tp$fun(pred = pred.bin))
  #tn
  tn.test = sum(tar.bin == pred.art.bin & pred.art.bin == 1L)
  tn.perf = performance(pred.bin, measures = tn, model = mod.bin)
  names(tn.perf) = NULL
  expect_equal(tn.test, tn.perf, tn$fun(pred = pred.bin))
  #fp
  fp.test = sum(tar.bin != pred.art.bin & pred.art.bin == 0L)
  fp.perf = performance(pred.bin, measures = fp, model = mod.bin)
  names(fp.perf) = NULL
  expect_equal(fp.test, fp.perf, fp$fun(pred = pred.bin))
  #fn
  fn.test = sum(tar.bin != pred.art.bin & pred.art.bin == 1L)
  fn.perf = performance(pred.bin, measures = fn, model = mod.bin)
  names(fn.perf) = NULL
  expect_equal(fn.test, fn.perf, fn$fun(pred = pred.bin))
  #tpr
  tpr.test = tp.test / sum(tar.bin == 0L)
  tpr.perf = performance(pred.bin, measures = tpr, model = mod.bin)
  names(tpr.perf) = NULL
  expect_equal(tpr.test, tpr.perf, tpr$fun(pred = pred.bin))
  #tnr
  tnr.test = tn.test / sum(tar.bin == 1L)
  tnr.perf = performance(pred.bin, measures = tnr, model = mod.bin)
  names(tnr.perf) = NULL
  expect_equal(tnr.test, tnr.perf, tnr$fun(pred = pred.bin))
  #fpr
  fpr.test = fp.test / sum(tar.bin != 0L)
  fpr.perf = performance(pred.bin, measures = fpr, model = mod.bin)
  names(fpr.perf) = NULL
  expect_equal(fpr.test, fpr.perf, fpr$fun(pred = pred.bin))
  #fnr
  fnr.test = fn.test / sum(tar.bin != 1L)
  fnr.perf = performance(pred.bin, measures = fnr, model = mod.bin)
  names(fnr.perf) = NULL
  expect_equal(fnr.test, fnr.perf, fnr$fun(pred = pred.bin))
  #ppv
  ppv.test = tp.test / sum(pred.art.bin == 0L)
  ppv.perf = performance(pred.bin, measures = ppv, model = mod.bin)
  names(ppv.perf) = NULL
  expect_equal(ppv.test, ppv.perf, ppv$fun(pred = pred.bin))
  #npv
  npv.test = tn.test / sum(pred.art.bin == 1L)
  npv.perf = performance(pred.bin, measures = npv, model = mod.bin)
  names(npv.perf) = NULL
  expect_equal(npv.test, npv.perf, npv$fun(pred = pred.bin))
  #fdr
  fdr.test = fp.test / sum(pred.art.bin == 0L)
  fdr.perf = performance(pred.bin, measures = fdr, model = mod.bin)
  names(fdr.perf) = NULL
  expect_equal(fdr.test, fdr.perf, fdr$fun(pred = pred.bin))
  #bac
  bac.test = 0.5 * (tpr.test / (tpr.test + fnr.test) + tnr.test /
    (tnr.test + fpr.test))
  bac.perf = performance(pred.bin, measures = bac, model = mod.bin)
  names(bac.perf) = NULL
  expect_equal(bac.test, bac.perf, bac$fun(pred = pred.bin))
  #ber
  ber.test = 1L - bac.test
  ber.perf = performance(pred.bin, measures = ber, model = mod.bin)
  names(ber.perf) = NULL
  expect_equal(ber.test, ber.perf, ber$fun(pred = pred.bin))
  #auc
  auc.test = (tpr.test + tnr.test) / 2L
  auc.perf = performance(pred.bin, measures = auc, model = mod.bin)
  names(auc.perf) = NULL
  expect_equal(auc.test, auc.perf, auc$fun(pred = pred.bin))
  #mcc
  mcc.test =  (tp.test * tn.test - fp.test * fn.test) /
    sqrt((tp.test + fp.test) * (tp.test + fn.test) *
      (tn.test + fp.test) * (tn.test + fn.test))
   mcc.perf = performance(pred.bin, measures = mcc, model = mod.bin)
  names(mcc.perf) = NULL
  expect_equal(mcc.test, mcc.perf, mcc$fun(pred = pred.bin))
  #f1
  f1.test = 2 * tp.test / (sum(tar.bin == 0L) + sum(pred.art.bin == 0L))
  f1.perf = performance(pred.bin, measures = f1, model = mod.bin)
  names(f1.perf) = NULL
  expect_equal(f1.test, f1.perf, f1$fun(pred = pred.bin))
  #gmean
  gmean.test = sqrt(tp.test * tn.test)
  gmean.perf = performance(pred.bin, measures = gmean, model = mod.bin)
  names(gmean.perf) = NULL
  expect_equal(gmean.test, gmean.perf, gmean$fun(pred = pred.bin))
  #gpr
  gpr.test = sqrt(ppv.test * tpr.test)
  gpr.perf = performance(pred.bin, measures = gpr, model = mod.bin)
  names(gpr.perf) = NULL
  expect_equal(gpr.test, gpr.perf, gpr$fun(pred = pred.bin))

  #test multilabel measures

  #hamloss
  hamloss.test = mean(c(tar1.multilabel != pred.multilabel$data[, 4L],
      tar2.multilabel != pred.multilabel$data[, 5L]))
  hamloss.perf = performance(pred.multilabel,
   measures = hamloss, model = mod.multilabel)
  names(hamloss.perf) = NULL
  expect_equal(hamloss.test, hamloss.perf, hamloss$fun(pred = pred.multilabel))
  #subset01
  subset01.test = mean((tar1.multilabel != pred.multilabel$data[, 4L]) ==
    (tar2.multilabel != pred.multilabel$data[, 5L]))
  subset01.perf = performance(pred.multilabel,
    measures = subset01, model = mod.multilabel)
  names(subset01.perf) = NULL
  expect_equal(subset01.test, subset01.perf, subset01$fun(pred = pred.multilabel))
  #f1mult
  f1mult.test = 2 * sum(tar1.multilabel * pred.multilabel$data[, 4L], tar2.multilabel * pred.multilabel$data[, 5L]) / 
    sum(c(tar1.multilabel, tar2.multilabel, pred.multilabel$data[, 4L], pred.multilabel$data[, 5L]))
  f1mult1.perf = performance(pred.multilabel,
    measures = f1mult, model = mod.multilabel)
  names(f1mult1.perf) = NULL
  expect_equal(f1mult.test, f1mult1.perf, f1mult$fun(pred = pred.multilabel))
  #jaccard
  jaccard.test = sum(tar1.multilabel * pred.multilabel$data[, 4L], tar2.multilabel * pred.multilabel$data[, 5L]) / 
    (sum(c(tar1.multilabel, tar2.multilabel, pred.multilabel$data[, 4L], pred.multilabel$data[, 5L])) -
    sum(tar1.multilabel * pred.multilabel$data[, 4L], tar2.multilabel * pred.multilabel$data[, 5L]))
  jaccard.perf = performance(pred.multilabel,
    measures = jaccard, model = mod.multilabel)
  names(jaccard.perf) = NULL
  expect_equal(jaccard.test, jaccard.perf, jaccard$fun(pred = pred.multilabel))
  
  #test survival measures

  #cindex
  pos = pred.surv$data[pred.surv$data$truth.event == TRUE, "response"]
  neg = pred.surv$data[pred.surv$data$truth.event == FALSE, "response"]
  cons = c(ifelse(pos[1L] > neg, 1L, 0L), ifelse(pos[2L] > neg, 1L, 0L))
  ties = c(ifelse(pos[1L] == neg, 0.5, 0), ifelse(pos[2L] == neg, 0.5, 0))
  n.pairs = length(pos) * length(neg)
  cindex.test = sum(c(cons, ties)) / n.pairs
  cindex.perf = performance(pred.surv, measures = cindex, model = mod.surv)
  names(cindex.perf) = NULL
  expect_equal(cindex.test, cindex.perf, cindex$fun(pred = pred.surv))

  #test cost-sensitive measures

  #meancosts
  meancosts.test = (0 + 0 + 0 + 1) / 4L
  meancosts.perf = performance(pred.costsens, measures = meancosts,
   model = mod.costsens, task = task.costsens)
  names(meancosts.perf) = NULL
  expect_equal(meancosts.test, meancosts.perf,
   meancosts$fun(pred = pred.costsens, task = task.costsens))
  #mcp
  mcp.test = meancosts.test - 0
  mcp.perf = performance(pred.costsens, measures = mcp,
    task = task.costsens, model = mod.costsens)
  names(mcp.perf) = NULL
  expect_equal(mcp.test, mcp.perf,
   mcp$fun(pred = pred.costsens, task = task.costsens))


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
  names(db.perf) = NULL
  expect_equal(db.test, db.perf, db$fun(task = task.cluster,
   pred = pred.cluster, feats = data.cluster))
  #dunn
  exdist = min(sqrt(sum((c(1, 3) - c(3, 1))^2)), sqrt(sum((c(2, 4) - c(3, 1))^2)),
    sqrt(sum((c(4, 3) - c(3, 2))^2)))
  indist = max(sqrt(sum((c(1, 3) - c(2, 4))^2)), sqrt(sum((c(1, 3) - c(4, 2))^2)),
    sqrt(sum((c(2, 4) - c(4, 2))^2)))
  dunn.test = exdist / indist
  dunn.perf = performance(pred.cluster, measures = dunn,
    model = mod.cluster, feats = data.cluster)
  names(dunn.perf) = NULL
  expect_equal(dunn.test, dunn.perf,
   dunn$fun(pred = pred.cluster, feats = data.cluster))
  #g1 index
  exsum = sqrt(sum((c(1, 3) - c(3, 1))^2)) + sqrt(sum((c(2, 4) - c(3, 1))^2)) +
    sqrt(sum((c(4, 3) - c(3, 2))^2))
  insum = sqrt(sum((c(1, 3) - c(2, 4))^2)) + sqrt(sum((c(1, 3) - c(4, 2))^2)) +
    sqrt(sum((c(2, 4) - c(4, 2))^2))
  g1.test = exsum / insum
  g1.perf = performance(pred.cluster, measures = G1,
    model = mod.cluster, feats = data.cluster)
  names(g1.perf) = NULL
  expect_equal(g1.test, g1.perf,
   G1$fun(pred = pred.cluster, feats = data.cluster))
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
  names(g2.perf) = NULL
  expect_equal(g2.test, g2.perf,
   G2$fun(pred = pred.cluster, feats = data.cluster))
  #silhouette
  dists = as.matrix(clusterSim::dist.GDM(data.cluster))
  ais = replace(dists, dists == 0, NA)[-3L, -3L]
  ais = apply(ais, MARGIN = 2L, mean, na.rm = TRUE)
  bis = dists[-3L, 3L]
  sil.data = data.frame(t(rbind(ais, bis)))
  sils = (sil.data$bis - sil.data$ais) / pmax(sil.data$bis, sil.data$ais)
  silhouette.test = sum(sils)/nrow(data.cluster)
  silhouette.perf = performance(pred.cluster, measures = silhouette,
    model = mod.cluster, feats = data.cluster)
  names(silhouette.perf) = NULL
  expect_equal(silhouette.test, silhouette.perf,
   silhouette$fun(pred = pred.cluster, feats = data.cluster), tolerance = 0.00001)
})
