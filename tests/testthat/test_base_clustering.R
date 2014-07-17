context("clustering")

test_that("clustering resample",  {
  rdesc = makeResampleDesc("Bootstrap", iters = 5)
  lrn = makeLearner("cluster.SimpleKMeans")
  res = resample(lrn, noclass.task, rdesc)

  expect_true(all(!is.na(res$measures.test)))
  expect_false(is.na(res$aggr))
})

test_that("clustering benchmark", {
  task.names = c("noclass")
  tasks = list(noclass.task)
  learner.names = c("cluster.SimpleKMeans")
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 3L)

  res = benchmark(learners = learners, task = tasks, resamplings = makeResampleDesc("CV", iters = 2L))
  expect_true("BenchmarkResult" %in% class(res))
})

test_that("clustering downsample", {
  down.tsk = downsample(noclass.task, perc = 1/3)
  expect_equal(down.tsk$task.desc$size, 50L)
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
