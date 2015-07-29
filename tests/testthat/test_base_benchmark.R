context("benchmark")

test_that("benchmark", {
  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  res = benchmark(learners = learners, task = tasks, resampling = rin)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(rin$iters * length(task.names) * length(learner.names), 4L))
  expect_true(setequal(df$task.id, task.names))
  expect_true(setequal(df$learner.id, learner.names))
  expect_true(is.numeric(df$mmce))
  expect_equal(getBMRTaskIds(res), task.names)
  expect_equal(getBMRLearnerIds(res), learner.names)
  expect_equal(getBMRMeasures(res), list(mmce))
  expect_equal(getBMRMeasureIds(res), "mmce")

  preds = getBMRPredictions(res, as.df = FALSE)
  expect_true(is.list(preds))
  expect_true(setequal(names(preds), task.names))
  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  expect_true(setequal(names(preds1), learner.names))
  preds11 = preds1[[1L]]
  expect_is(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_is(preds, "data.frame")
  expect_equal(nrow(preds), 2 * (getTaskSize(multiclass.task) + getTaskSize(binaryclass.task)))

  p = getBMRPerformances(res, as.df = TRUE)
  expect_is(p, "data.frame")
  expect_equal(nrow(p), length(task.names) * length(learner.names) * rin$iters)

  a = getBMRAggrPerformances(res, as.df = TRUE)
  expect_is(a, "data.frame")
  expect_equal(nrow(a), length(task.names) * length(learner.names))

  # make it more complex
  ps = makeParamSet(makeDiscreteLearnerParam("cp", values = c(0.01, 0.1)))
  learner.names = c("classif.lda", "classif.rpart", "classif.lda.featsel", "classif.rpart.tuned", "classif.lda.filtered")
  learners = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  learners = c(learners, list(
    makeFeatSelWrapper(learners[[1L]], resampling = rin, control = makeFeatSelControlRandom(maxit = 3)),
    makeTuneWrapper(learners[[2L]], resampling = rin, par.set = ps, control = makeTuneControlGrid()),
    makeFilterWrapper(learners[[1L]], fw.perc = 0.5)
  ))
  resamplings = list(rin, makeResampleDesc("Bootstrap", iters = 2L))
  measures = list(mmce, acc)

  res = benchmark(learners = learners, tasks = tasks, resamplings = resamplings, measures = measures)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(rin$iters * length(task.names) * length(learner.names), 5L))
  expect_true(setequal(df$task.id, task.names))
  expect_true(setequal(df$learner.id, learner.names))
  expect_true(is.numeric(df$mmce))
  expect_true(is.numeric(df$acc))
  expect_equal(getBMRTaskIds(res), task.names)
  expect_equal(getBMRLearnerIds(res), learner.names)

  preds = getBMRPredictions(res, as.df = FALSE)
  expect_true(is.list(preds))
  expect_true(setequal(names(preds), task.names))
  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  expect_true(setequal(names(preds1), learner.names))
  preds11 = preds1[[1L]]
  expect_is(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_is(preds, "data.frame")

  p = getBMRPerformances(res, as.df = TRUE)
  expect_is(p, "data.frame")
  expect_equal(nrow(p), length(task.names) * length(learner.names) * rin$iters)

  a = getBMRAggrPerformances(res, as.df = TRUE)
  expect_is(a, "data.frame")
  expect_equal(nrow(a), length(task.names) * length(learner.names))

  tr = getBMRTuneResults(res, as.df = FALSE)

  trd = getBMRTuneResults(res, as.df = TRUE)
  expect_is(trd, "data.frame")
  expect_equal(ncol(trd), 5)
  expect_equal(nrow(trd), 4)

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

  f(getBMRFeatSelResults(res), "FeatSelResult")
  f(getBMRTuneResults(res), "TuneResult")
  f(getBMRFilteredFeatures(res), "character")
})
