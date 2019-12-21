context("clustering")

test_that("clustering predict", {
  lrn = makeLearner("cluster.cmeans", predict.type = "prob")
  model = train(lrn, noclass.task)
  pred = predict(model, task = noclass.task)
  y = pred$data$response
  expect_true(is.integer(y))

  p = getPredictionProbabilities(pred)
  expect_true(is.data.frame(p) && nrow(noclass.df) && ncol(p) == max(y))
})


test_that("clustering performance", {
  lrn = makeLearner("cluster.SimpleKMeans")
  model = train(lrn, noclass.task)
  pred = predict(model, task = noclass.task)

  expect_true(is.numeric(performance(pred, task = noclass.task, measures = db)))
  expect_true(is.numeric(performance(pred, task = noclass.task, measures = dunn)))
  expect_true(is.numeric(performance(pred, task = noclass.task, measures = G1)))
  expect_true(is.numeric(performance(pred, task = noclass.task, measures = G2)))
  expect_true(is.numeric(performance(pred, task = noclass.task, measures = silhouette)))
})

test_that("clustering performance with missing clusters", {
  lrn = makeLearner("cluster.SimpleKMeans")
  model = train(lrn, noclass.task)
  pred = predict(model, task = noclass.task)
  pred$data$response = sample(c(1, 3, 4), length(pred$data$response), replace = TRUE)

  expect_warning(performance(pred, task = noclass.task, measures = db), NA)
  expect_warning(performance(pred, task = noclass.task, measures = dunn), NA)
  expect_warning(performance(pred, task = noclass.task, measures = G1), NA)
  expect_warning(performance(pred, task = noclass.task, measures = G2), NA)
  expect_warning(performance(pred, task = noclass.task, measures = silhouette), NA)
})

test_that("clustering resample", {
  rdesc = makeResampleDesc("Subsample", split = 0.3, iters = 2)
  lrn = makeLearner("cluster.SimpleKMeans")
  res = resample(lrn, noclass.task, rdesc)

  expect_true(all(!is.na(res$measures.test)))
  expect_false(is.na(res$aggr))
})

test_that("clustering benchmark", {
  task.names = "noclass"
  tasks = list(noclass.task)
  learner.names = "cluster.SimpleKMeans"
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  res = benchmark(learners = learners, task = tasks, resamplings = makeResampleDesc("CV", iters = 2L))
  expect_true("BenchmarkResult" %in% class(res))
})

test_that("clustering downsample", {
  down.tsk = downsample(noclass.task, perc = 1 / 3)
  expect_equal(getTaskSize(down.tsk), 50L)
})

test_that("clustering tune", {
  lrn = makeLearner("cluster.SimpleKMeans")
  rdesc = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeIntegerParam("N", lower = 2, upper = 10)
  )

  ctrl = makeTuneControlRandom(maxit = 2)
  tr = tuneParams(lrn, noclass.task, rdesc, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), 2)
  expect_true(!is.na(tr$y))
})
