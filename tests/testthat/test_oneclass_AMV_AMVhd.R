context("oneclass_measures_AMV_AMVhd")

test_that("AMV", {
  # creates an AMV measure which calculates the area under the curve between 0.8 and 0.99
  # with 50 steps.
  amv = makeAMVMeasure(id = "AMV", minimize = TRUE, alphas = c(0.8, 0.99),
    n.alpha = 50, n.sim = 10e4, best = 0, worst = NULL)
  expect_equal(amv$id, "AMV")
  expect_equal(amv$extra.args[[1]], c(0.8, 0.99))
  expect_equal(amv$extra.args[[2]], 10e4)

  data = getTaskData(oneclass2d.task)
  inds.split = chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
  train.inds = inds.split[[1]]
  test.inds = inds.split[[2]]
  lrn = makeLearner("oneclass.svm", predict.type = "prob")
  mod = train(lrn, oneclass2d.task, subset = train.inds)
  pred = predict(mod, oneclass2d.task, subset = test.inds)

  # calculate performance for prediction object, pass data of features used for
  # prediction as feats in performance
  perf = performance(pred, measures = list(amv), model = mod, feats = data[test.inds, 1:2])
  expect_numeric(perf)
  expect_equal(names(perf), "AMV")
  expect_true(perf >= 0)
})


test_that("AMVhd", {
  # creates anomaly data with feature size nine
  set.seed(123)
  sigma = matrix(0, 9, 9)
  diag(sigma) = c(4, 5, 8, 3, 2, 6, 9, 3, 1)
  normal = MASS::mvrnorm(n = 1000, rep(0, 9), sigma)
  colnames(normal) = paste0("V", 1:9)
  normal = as.data.frame(normal)
  normal$normal = TRUE

  anomaly = matrix(sample(size = 50 * 9, x = 20:100, replace = TRUE), 50, 9)
  colnames(anomaly) = paste0("V", 1:9)
  anomaly = as.data.frame(anomaly)
  anomaly$normal = FALSE
  data = rbind(normal, anomaly)
  data = na.omit(data)

  # create train and test sets
  inds.split = chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
  train.inds = inds.split[[1]]
  test.inds = inds.split[[2]]

  # creates an AMVhd measure which calculates the area under the curve between 0.8 and 0.99
  # with 50 steps for high dimensional data.
  amvhd = makeAMVhdMeasure(id = "AMVhd", minimize = TRUE, alphas = c(0.8, 0.99),
    n.alpha = 50, n.sim = 10e1, best = 0, worst = NULL)

  expect_equal(amvhd$id, "AMVhd")
  expect_equal(amvhd$extra.args[[1]], c(0.8, 0.99))
  expect_equal(amvhd$extra.args[[2]], 10e1)

  task = makeOneClassTask(data = data, target = "normal", positive = "TRUE", negative = "FALSE")
  # base learner
  lrn = makeLearner("oneclass.svm", predict.type = "prob")
  mod = train(lrn, task, subset = train.inds)
  pred = predict(mod, task, subset = test.inds)

  # calculate AMVhd performance
  perf = performance(pred = pred, measures = list(amvhd), model = mod, task = task, feats = data[test.inds, 1:9])
  expect_numeric(perf)
  expect_equal(names(perf), "AMVhd")
  expect_true(perf >= 0)
})
