context("oneclass measures")

test_that("precision measures", {
  # creates an R-precision measure which calculates the precision for the top 5 (=p) ranks
  rprecision = makePrecisionMeasure(id = "RPrecision", minimize = FALSE,
    best = 0, worst = NULL, type = "rprecision", adjusted = FALSE)

  # creates an P@5 measure which calculates the precision for the top 5 (=p) ranks
  precisionat5 = makePrecisionMeasure(id = "Precisionat5", minimize = FALSE,
    best = 0, worst = NULL, p = 5, type = "precisionatp", adjusted = FALSE)

  # creates an average precision measure which calculates the average precision
  # for all ranks of possible choices of p in \{1, 2, ..., number of anomalies\}.
  avgprecision = makePrecisionMeasure(id = "AvgPrecision", minimize = FALSE,
    best = 0, worst = NULL, type = "avgprecision", adjusted = FALSE)

  lrn = makeLearner("oneclass.svm", predict.type = "prob", nu = 0.05)
  mod = train(lrn, oneclass2d.task)
  pred = predict(mod, task = oneclass2d.task)

  # calculate performance for prediction object
  perf = performance(pred = pred, measures = list(rprecision, precisionat5,
    avgprecision), model = mod, task = task)

  expect_true(perf >= 0 && perf <= 1)
  expect_numeric(perf)
  expect_equal(names(perf), c("RPrecision", "Precisionat5", "AvgPrecision"))

  precisionat100 = makePrecisionMeasure(id = "Precisionat5", minimize = FALSE,
    best = 0, worst = NULL, p = 100, type = "precisionatp", adjusted = FALSE)
  expect_warning(performance(pred = pred, measures = list(precisionat100), model = mod, task = task), "p should lie in")
})


test_that("calculate precision measures", {
  # creates an R-precision measure which calculates the precision for the top 5 (=p) ranks
  rprecision = makePrecisionMeasure(id = "RPrecision", minimize = FALSE,
    best = 0, worst = NULL, type = "rprecision", adjusted = FALSE)

  # creates an P@5 measure which calculates the precision for the top 5 (=p) ranks
  precisionat5 = makePrecisionMeasure(id = "Precisionat5", minimize = FALSE,
    best = 0, worst = NULL, p = 5, type = "precisionatp", adjusted = FALSE)

  # creates an average precision measure which calculates the average precision
  # for all ranks of possible choices of p in \{1, 2, ..., number of anomalies\}.
  avgprecision = makePrecisionMeasure(id = "AvgPrecision", minimize = FALSE,
    best = 0, worst = NULL, type = "avgprecision", adjusted = FALSE)

  lrn = makeLearner("oneclass.svm", predict.type = "prob", nu = 0.05)
  mod = train(lrn, oneclass2d.task)
  pred = predict(mod, task = oneclass2d.task)

  # calculate performance for prediction object
  perf = performance(pred = pred, measures = list(rprecision, precisionat5,
    avgprecision), model = mod, task = task)

  n.anomaly = sum(pred$data$truth == pred$task.desc$positive)
  scores = pred$data[, 3]
  rank = order(scores, decreasing = TRUE)
  ind.true = which(pred$data$truth == pred$task.desc$positive)

  # precision@5
  expect_equal(sum(rank[ind.true] <= 5) / 5, as.numeric(perf[2]))

  # rprecision
  expect_equal(sum(rank[ind.true] <= n.anomaly) / n.anomaly, as.numeric(perf[1]))

  # avg precision
  p = 1:n.anomaly
  expect_equal(mean(unlist(lapply(p, FUN = function(x) {sum(rank[ind.true] <= x) / x}))), as.numeric(perf[3]))
})


test_that("wac measures", {

  var1 = c(1, 2, 3, 4)
  var2 = c(3, 4, 1, 2)
  tar.bin = factor(c(1L, 0L, 0L, 1L))
  pred.art.bin = factor(c(1L, 1L, 0L, 0L))
  data.bin = data.frame(var1, var2, tar.bin)
  task.bin = makeOneClassTask(data = data.bin, target = "tar.bin", positive = 1, negative = 0)
  lrn.bin = makeLearner("oneclass.svm")
  mod.bin = train(lrn.bin, task.bin)
  pred.bin = predict(mod.bin, task.bin)

  tnr = measureTNR(tar.bin, pred.art.bin, negative = 0)
  tpr = measureTPR(tar.bin, pred.art.bin, positive = 1)
  fnr = measureFNR(tar.bin, pred.art.bin, positive = 1, negative = 0)
  fpr = measureFPR(tar.bin, pred.art.bin, positive = 1, negative = 0)

  # weight
  w = 0.6
  # wac by hand
  wac.test = w * (tpr / (tpr + fnr) + (1-w) * tnr / (tnr + fpr))

  # wac with performance
  wac = makeWACMeasure(id = "wac", minimize = FALSE, best = 0, worst = NULL, w = w)
  wac.perf = performance(pred.bin, measures = wac, model = mod.bin)
  expect_equal(wac.test, as.numeric(wac.perf))
})

