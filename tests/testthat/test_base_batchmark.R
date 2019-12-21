context("batchmark")

prev = getOption("batchtools.verbose")
options(batchtools.verbose = FALSE)

test_that("batchmark", {
  skip_if_not_installed("batchtools")
  library(batchtools)
  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE, seed = 1)

  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  ids = batchmark(learners = makeLearner("classif.lda", predict.type = "prob"),
                  tasks = binaryclass.task, resamplings = rin, reg = reg)

  expect_data_table(ids, ncol = 1L, nrow = 2, key = "job.id")
  expect_set_equal(ids$job.id, 1:2)

  tab = summarizeExperiments(reg = reg)
  expect_equal(tab$problem, "binary")
  expect_equal(tab$algorithm, "classif.lda")
  expect_set_equal(findExperiments(reg = reg)$job.id, 1:2)

  submitJobs(reg = reg)
  expect_true(waitForJobs(reg = reg))

  res = reduceBatchmarkResults(reg = reg)
  preds = getBMRPredictions(res, as.df = FALSE)
  expect_true(is.list(preds))
  expect_true(setequal(names(preds), "binary"))

  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  expect_true(setequal(names(preds1), "classif.lda"))

  preds11 = preds1[[1L]]
  expect_is(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_is(preds, "data.frame")
  expect_equal(nrow(preds), getTaskSize(binaryclass.task))
  expect_equal(ncol(preds), 9)
  expect_equal(unique(preds$iter), 1:2)

  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE, seed = 1)
  res = batchmark(learners = learners, tasks = tasks, resamplings = rin, reg = reg)
  submitJobs(reg = reg)
  expect_true(waitForJobs(reg = reg))

  res = reduceBatchmarkResults(reg = reg)

  # check if keep.extract = FALSE really throws away the tuning results
  expect_true(all(sapply(res$results$binary[[1]]$extract, function(x) is.null(x))))

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
  learner.names = c("classif.lda", "classif.rpart", "classif.lda.featsel",
                    "classif.rpart.tuned", "classif.lda.filtered")
  learners = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
  learners = c(learners, list(
    makeFeatSelWrapper(learners[[1L]], resampling = rin, control = makeFeatSelControlRandom(maxit = 3)),
    makeTuneWrapper(learners[[2L]], resampling = rin, par.set = ps, control = makeTuneControlGrid()),
    makeFilterWrapper(learners[[1L]], fw.perc = 0.5)
  ))
  resamplings = list(rin, makeResampleDesc("Bootstrap", iters = 2L))
  measures = list(mmce, acc)

  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE, seed = 1)
  res = batchmark(learners = learners, tasks = tasks, resamplings = resamplings,
    measures = measures, reg = reg, keep.extract = TRUE)
  submitJobs(reg = reg)
  expect_true(waitForJobs(reg = reg))
  expect_data_table(findErrors(reg = reg), nrow = 0L)
  res = reduceBatchmarkResults(reg = reg)

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
  expect_is(tr, "list")
  expect_equal(length(tr), 2)
  expect_true(setequal(names(tr), task.names))

  tr1 = tr[[task.names[1L]]]
  expect_true(is.list(tr1))
  expect_true(setequal(names(tr1), learner.names))

  tr11 = tr1[[paste0(learner.names[2L], ".tuned")]]
  expect_equal(length(tr11), 2)

  tr111 = tr11[[1L]]
  expect_is(tr111, "TuneResult")

  trd = getBMRTuneResults(res, as.df = TRUE)
  expect_is(trd, "data.frame")
  expect_equal(ncol(trd), 5)
  expect_equal(nrow(trd), 4)
  expect_equal(unique(trd$task.id), factor(task.names))
  expect_equal(unique(trd$learner.id), factor("classif.rpart.tuned"))
  expect_equal(unique(trd$iter), 1:2)

  tf = getBMRFeatSelResults(res, as.df = FALSE)
  expect_is(tf, "list")
  expect_equal(length(tf), 2)
  expect_true(setequal(names(tf), task.names))

  tf1 = tf[[task.names[1L]]]
  expect_true(is.list(tf1))
  expect_true(setequal(names(tf1), learner.names))

  tf11 = tf1[[paste0(learner.names[1L], ".featsel")]]
  expect_equal(length(tf11), 2)

  tf111 = tf11[[1L]]
  expect_is(tf111, "FeatSelResult")

  tfd = getBMRFeatSelResults(res, as.df = TRUE)
  expect_is(tfd, "data.frame")
  expect_equal(ncol(tfd), 4)
  feats = c(
    lapply(tf$binary$classif.lda.featsel, function(x) x$x),
    lapply(tf$multiclass$classif.lda.featsel, function(x) x$x)
  )
  expect_equal(nrow(tfd), sum(lengths(feats)))
  expect_equal(unique(tfd$task.id), factor(task.names))
  expect_equal(unique(tfd$learner.id), factor("classif.lda.featsel"))
  expect_equal(unique(tfd$iter), 1:2)

  tff = getBMRFilteredFeatures(res, as.df = FALSE)
  expect_is(tff, "list")
  expect_equal(length(tff), 2)
  expect_true(setequal(names(tff), task.names))
  tff1 = tff[[task.names[1L]]]
  expect_true(is.list(tff1))
  expect_true(setequal(names(tff1), learner.names))
  tff11 = tff1[[paste0(learner.names[1L], ".filtered")]]
  expect_equal(length(tff11), 2)
  tff111 = tff11[[1L]]
  expect_is(tff111, "character")

  tffd = getBMRFilteredFeatures(res, as.df = TRUE)
  expect_is(tffd, "data.frame")
  expect_equal(ncol(tffd), 4)
  expect_equal(nrow(tffd), 64)
  expect_equal(unique(tffd$task.id), factor(task.names))
  expect_equal(unique(tffd$learner.id), factor("classif.lda.filtered"))
  expect_equal(unique(tffd$iter), 1:2)

  f = function(tmp, cl) {

    context(sprintf("batchmark: extracting %s", cl))
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

test_that("keep.preds and models are passed down to resample()", {
  skip_if_not_installed("batchtools")
  library(batchtools)
  task.names = "binary"
  tasks = list(binaryclass.task)
  learner.names = "classif.lda"
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE)
  res = batchmark(learners = makeLearner("classif.lda", predict.type = "prob"), task = binaryclass.task, resampling = rin, models = TRUE, reg = reg)
  submitJobs(reg = reg)
  expect_true(waitForJobs(reg = reg))
  res = reduceBatchmarkResults(reg = reg, keep.pred = TRUE)
  x = res$results$binary$classif.lda
  expect_is(x, "ResampleResult")
  expect_list(x$models, types = "WrappedModel")
  expect_is(x$pred, "ResamplePrediction")

  ## test getter function for models
  models = getBMRModels(res)
  expect_true(is.list(models))
  expect_true(setequal(names(models), "binary"))

  models1 = models[[1L]]
  expect_true(is.list(models1))
  expect_true(setequal(names(models1), "classif.lda"))

  models11 = models1[[1L]]
  expect_true(is.list(models11))
  expect_equal(length(models11), 2L)

  models111 = models11[[1L]]
  expect_is(models111, "WrappedModel")

  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE)
  res = batchmark(learners = makeLearner("classif.lda", predict.type = "prob"),
                  tasks = binaryclass.task, resamplings = rin, models = FALSE, reg = reg)
  submitJobs(reg = reg)
  expect_true(waitForJobs(reg = reg))

  res = reduceBatchmarkResults(reg = reg, keep.pred = FALSE)
  x = res$results$binary$classif.lda
  models11 = getBMRModels(res)[[1L]][[1L]]
  expect_is(x, "ResampleResult")
  expect_null(x$pred)
  expect_null(models11)
})

test_that("batchmark works with resampling instances", {
  requirePackagesOrSkip("batchtools")
  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE, seed = 1)
  task = binaryclass.task
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rdesc = makeResampleDesc("CV", iters = 2L)
  rin = makeResampleInstance(rdesc, task)
  ids = batchmark(learners = learners, tasks = task, resamplings = rin, reg = reg)
  expect_data_table(ids, nrow = 4)
})

test_that("batchmark works with incomplete results", {
  requirePackagesOrSkip("batchtools")
  library(batchtools)
  reg = makeExperimentRegistry(file.dir = NA, make.default = FALSE)
  task = binaryclass.task
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rdesc = makeResampleDesc("CV", iters = 4L)
  rin = makeResampleInstance(rdesc, task)
  ids = batchmark(learners = learners, tasks = task, resamplings = rin, reg = reg)
  submitJobs(1:6, reg = reg)
  expect_true(waitForJobs(reg = reg))
  expect_warning({
    res = reduceBatchmarkResults(ids = 1:6, reg = reg, keep.pred = FALSE)
  }, "subset")
  expect_set_equal(getBMRLearnerIds(res), c("classif.lda", "classif.rpart"))

  expect_warning({
    res = reduceBatchmarkResults(ids = 1:3, reg = reg, keep.pred = FALSE)
  }, "subset")
  expect_set_equal(getBMRLearnerIds(res), "classif.lda")

  expect_warning({
    res = reduceBatchmarkResults(ids = data.table(job.id = 5), reg = reg, keep.pred = FALSE)
  }, "subset")
  expect_set_equal(getBMRLearnerIds(res), "classif.rpart")
})

options(batchtools.verbose = prev)
