context("benchmark")

test_that("benchmark", {
  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 3L)

  res = benchmark(learners = learners, task = tasks)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(4L, 3L))
  expect_true(setequal(df$task, task.names))
  expect_true(setequal(df$learner, learner.names))
  expect_true(is.numeric(df$mmce.test.mean))

  tmp = getPredictions(res)
  expect_true(is.list(tmp))
  expect_true(setequal(names(tmp), task.names))
  tmp = tmp[[1L]]
  expect_true(is.data.frame(tmp))


  # make it more complex
  ps = makeParamSet(makeDiscreteLearnerParam("cp", values=c(0.01, 0.1)))
  learner.names = c("classif.lda", "classif.rpart", "classif.lda.featsel", "classif.rpart.tuned", "classif.lda.filtered")
  learners = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  learners = c(learners, list(
    makeFeatSelWrapper(learners[[1L]], resampling = rin, control = makeFeatSelControlRandom(maxit = 3)),
    makeTuneWrapper(learners[[2L]], resampling = rin, par.set = ps, control = makeTuneControlGrid()),
    makeFilterWrapper(learners[[1L]], fw.val = 0.5)
  ))
  resamplings = list(rin, makeResampleDesc("Bootstrap", iters=3))
  measures = list(mmce, acc)

  res = benchmark(learners = learners, tasks = tasks, resamplings = resamplings, measures = measures)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(10L, 4L))
  expect_true(setequal(df$task, task.names))
  expect_true(setequal(df$learner, learner.names))
  expect_true(is.numeric(df$mmce.test.mean))
  expect_true(is.numeric(df$acc.test.mean))

  f = function(tmp, cl) {
    context(sprintf("benchmark: extracting %s", cl))
    expect_true(is.list(tmp))
    expect_true(setequal(names(tmp), task.names))
    tmp = tmp[[1L]]
    expect_equal(length(tmp), length(learners))
    tmp = Filter(Negate(is.null), tmp)
    expect_equal(length(tmp), 1L)
    tmp = tmp[[1L]]
    expect_true(inherits(tmp[[1L]], cl))
  }

  f(getFeatSelResult(res), "FeatSelResult")
  f(getTuneResult(res), "TuneResult")
  f(getFilterResult(res), "FilterResult")
})
