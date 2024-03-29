
test_that("benchmark", {
  set.seed(getOption("mlr.debug.seed"))

  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  res = benchmark(
    learners = makeLearner("classif.lda", predict.type = "prob"),
    task = binaryclass.task, resamplings = rin)
  preds = getBMRPredictions(res, as.df = FALSE)
  expect_true(is.list(preds))
  expect_true(setequal(names(preds), "binary"))
  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  expect_true(setequal(names(preds1), "classif.lda"))
  preds11 = preds1[[1L]]
  expect_s3_class(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), getTaskSize(binaryclass.task))
  expect_equal(ncol(preds), 9)
  expect_equal(unique(preds$iter), 1:2)

  res = benchmark(learners = learners, task = tasks, resampling = rin)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(rin$iters * length(task.names) *
    length(learner.names), 4L))
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
  expect_s3_class(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 2 * (getTaskSize(multiclass.task) +
    getTaskSize(binaryclass.task)))

  p = getBMRPerformances(res, as.df = TRUE)
  expect_s3_class(p, "data.frame")
  expect_equal(nrow(p), length(task.names) * length(learner.names) * rin$iters)

  a = getBMRAggrPerformances(res, as.df = TRUE)
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), length(task.names) * length(learner.names))

  # make it more complex
  ps = makeParamSet(makeDiscreteLearnerParam("cp", values = c(0.01, 0.1)))
  learner.names = c(
    "classif.lda", "classif.rpart", "classif.lda.featsel",
    "classif.rpart.tuned", "classif.lda.filtered")
  learners = list(
    makeLearner("classif.lda"),
    makeLearner("classif.rpart"))
  learners = c(learners, list(
    makeFeatSelWrapper(learners[[1L]],
      resampling = rin,
      control = makeFeatSelControlRandom(maxit = 3)),
    makeTuneWrapper(learners[[2L]],
      resampling = rin, par.set = ps,
      control = makeTuneControlGrid()),
    makeFilterWrapper(learners[[1L]], fw.perc = 0.5)
  ))
  resamplings = list(rin, makeResampleDesc("Bootstrap", iters = 2L))
  measures = list(mmce, acc)

  res = benchmark(
    learners = learners, tasks = tasks, resamplings = resamplings,
    measures = measures, keep.extract = TRUE)
  expect_true("BenchmarkResult" %in% class(res))

  df = as.data.frame(res)
  expect_true(is.data.frame(df))
  expect_equal(dim(df), c(rin$iters * length(task.names) *
    length(learner.names), 5L))
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
  expect_s3_class(preds11, "Prediction")

  preds = getBMRPredictions(res, as.df = TRUE)
  expect_s3_class(preds, "data.frame")

  p = getBMRPerformances(res, as.df = TRUE)
  expect_s3_class(p, "data.frame")
  expect_equal(nrow(p), length(task.names) * length(learner.names) * rin$iters)

  a = getBMRAggrPerformances(res, as.df = TRUE)
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), length(task.names) * length(learner.names))

  tr = getBMRTuneResults(res, as.df = FALSE)
  expect_list(tr)
  expect_equal(length(tr), 2)
  expect_true(setequal(names(tr), task.names))
  tr1 = tr[[task.names[1L]]]
  expect_true(is.list(tr1))
  expect_true(setequal(names(tr1), learner.names))
  tr11 = tr1[[paste0(learner.names[2L], ".tuned")]]
  expect_equal(length(tr11), 2)
  tr111 = tr11[[1L]]
  expect_s3_class(tr111, "TuneResult")

  trd = getBMRTuneResults(res, as.df = TRUE)
  expect_s3_class(trd, "data.frame")
  expect_equal(ncol(trd), 5)
  expect_equal(nrow(trd), 4)
  expect_equal(levels(as.factor(trd$task.id)), task.names)
  expect_equal(levels(as.factor(trd$learner.id)), "classif.rpart.tuned")
  expect_equal(unique(trd$iter), 1:2)

  tf = getBMRFeatSelResults(res, as.df = FALSE)
  expect_list(tf)
  expect_equal(length(tf), 2)
  expect_true(setequal(names(tf), task.names))
  tf1 = tf[[task.names[1L]]]
  expect_true(is.list(tf1))
  expect_true(setequal(names(tf1), learner.names))
  tf11 = tf1[[paste0(learner.names[1L], ".featsel")]]
  expect_equal(length(tf11), 2)
  tf111 = tf11[[1L]]
  expect_s3_class(tf111, "FeatSelResult")

  tfd = getBMRFeatSelResults(res, as.df = TRUE)
  expect_s3_class(tfd, "data.frame")
  expect_equal(ncol(tfd), 4)
  expect_gt(nrow(tfd), 10)
  expect_equal(levels(as.factor(tfd$task.id)), task.names)
  expect_equal(levels(as.factor(tfd$learner.id)), "classif.lda.featsel")
  expect_equal(unique(tfd$iter), 1:2)

  tff = getBMRFilteredFeatures(res, as.df = FALSE)
  expect_list(tff)
  expect_equal(length(tff), 2)
  expect_true(setequal(names(tff), task.names))
  tff1 = tff[[task.names[1L]]]
  expect_true(is.list(tff1))
  expect_true(setequal(names(tff1), learner.names))
  tff11 = tff1[[paste0(learner.names[1L], ".filtered")]]
  expect_equal(length(tff11), 2)
  tff111 = tff11[[1L]]
  expect_type(tff111, "character")

  tffd = getBMRFilteredFeatures(res, as.df = TRUE)
  expect_s3_class(tffd, "data.frame")
  expect_equal(ncol(tffd), 4)
  expect_equal(nrow(tffd), 64)
  expect_equal(levels(as.factor(tffd$task.id)), task.names)
  expect_equal(levels(as.factor(tffd$learner.id)), "classif.lda.filtered")
  expect_equal(unique(tffd$iter), 1:2)

  f = function(tmp, cl) {
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
  task.names = "binary"
  tasks = list(binaryclass.task)
  learner.names = "classif.lda"
  learners = lapply(learner.names, makeLearner)
  rin = makeResampleDesc("CV", iters = 2L)

  res = benchmark(
    learners = makeLearner("classif.lda", predict.type = "prob"),
    task = binaryclass.task, resampling = rin, keep.pred = TRUE, models = TRUE)
  x = res$results$binary$classif.lda
  expect_s3_class(x, "ResampleResult")
  expect_list(x$models, types = "WrappedModel")
  expect_s3_class(x$pred, "ResamplePrediction")

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
  expect_s3_class(models111, "WrappedModel")

  res = benchmark(
    learners = makeLearner("classif.lda", predict.type = "prob"),
    task = binaryclass.task, resampling = rin, keep.pred = FALSE,
    models = FALSE)
  x = res$results$binary$classif.lda
  models11 = getBMRModels(res)[[1L]][[1L]]
  expect_s3_class(x, "ResampleResult")
  expect_null(x$pred)
  expect_null(models11)
})

test_that("benchmark work with learner string", {
  # we had a bug here, check that learner(s) are created from string
  b = benchmark("classif.rpart", iris.task, hout)
  expect_class(b, "BenchmarkResult")
  b = benchmark(c("classif.rpart", "classif.lda"), iris.task, hout)
  expect_class(b, "BenchmarkResult")
})

test_that("drop option works for BenchmarkResults_operators", {

  # setup bmrs for checking
  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  learner.names = c("classif.lda", "classif.rpart")
  learners = lapply(learner.names, makeLearner)
  two_two = benchmark(learners = learners, task = tasks, resampling = hout)
  two_one = benchmark(
    learners = learners[[1L]], task = tasks,
    resampling = hout)
  one_two = benchmark(
    learners = learners, task = tasks[[1L]],
    resampling = hout)
  one_one = benchmark(
    learners = learners[[1L]], task = tasks[[1L]],
    resampling = hout)

  # check behavior extensively
  res = getBMRPredictions(two_two, drop = TRUE)
  expect_true(all(names(res) == task.names))

  res = getBMRPredictions(two_one, drop = TRUE)
  expect_true(all(names(res) == task.names))

  res = getBMRPredictions(one_two, drop = TRUE)
  expect_true(all(names(res) == learner.names))

  res = getBMRPredictions(one_one, drop = TRUE)
  expect_class(res, "Prediction")

  # check all other functions that use 'drop' briefly
  testDropOption = function(bmr, fun, new.names, ...) {
    extra.args = list(...)
    res = do.call(fun, c(list(bmr, drop = TRUE), extra.args))
    expect_true(all(names(res) == new.names))
  }

  funs_to_test = c(
    getBMRPerformances, getBMRTuneResults,
    getBMRFeatSelResults, getBMRFilteredFeatures, getBMRModels)
  lapply(funs_to_test,
    FUN = testDropOption, bmr = one_two,
    new.names = learner.names)

  # getBMROptResults needs separate checking since it needs extra argument
  testDropOption(one_two, getBMROptResults,
    new.names = learner.names,
    wrapper.class = "cl")
})

test_that("benchmark works with ensemble filters", {
  lrn = makeLearner("classif.ksvm")
  lrn = makeFilterWrapper(lrn,
    fw.method = "E-Borda",
    fw.base.methods = c("anova.test", "variance"))

  par.set = makeParamSet(
    makeNumericParam("C",
      lower = -2, upper = 2,
      trafo = function(x) 2^x),
    makeNumericParam("sigma",
      lower = -2, upper = 2,
      trafo = function(x) 2^x),
    makeNumericParam("fw.perc", lower = 0, upper = 1)
  )

  task.names = c("binary", "multiclass")
  tasks = list(binaryclass.task, multiclass.task)
  rin = makeResampleDesc("CV", iters = 2L)
  tune.ctrl = makeTuneControlRandom(maxit = 3)

  tune_wrapper_svm = makeTuneWrapper(lrn,
    resampling = rin, par.set = par.set,
    control = tune.ctrl, show.info = FALSE,
    measures = list(acc))

  expect_class(benchmark(
    learners = tune_wrapper_svm, task = tasks,
    resampling = rin, measures = list(acc)), "BenchmarkResult")
})
test_that("benchmark handles failure models correctly", {

  # RWeka not avail
  skip_on_cran()
  skip_on_os("windows")

  # Define task
  task = binaryclass.task

  # Define filter parameter set
  filter.ps = makeParamSet(makeIntegerParam("fw.abs",
    lower = 1,
    upper = getTaskNFeats(task)))

  # Define tuning control
  ctrl = makeTuneControlRandom(maxit = 10L)

  # Define resampling strategies
  inner = mlr::makeResampleDesc("CV", stratify = FALSE, iters = 2L)
  outer = mlr::makeResampleDesc("CV", stratify = FALSE, iters = 2L)

  # Define learner
  quiet_learner = makeLearner("classif.__mlrmocklearners__3",
    config = list("on.learner.error" = "quiet"))
  quiet_learner = makeFilterWrapper(quiet_learner,
    fw.method = "FSelector_chi.squared")

  quiet_learner = makeTuneWrapper(quiet_learner,
    resampling = inner,
    control = ctrl,
    par.set = filter.ps, show.info = TRUE)

  stop.learner = makeLearner("classif.__mlrmocklearners__3",
    config = list("on.learner.error" = "stop"))
  stop.learner = makeFilterWrapper(stop.learner,
    fw.method = "FSelector_chi.squared")

  stop.learner = makeTuneWrapper(stop.learner,
    resampling = inner,
    control = ctrl,
    par.set = filter.ps, show.info = TRUE)

  warn_learner = makeLearner("classif.__mlrmocklearners__3",
    config = list("on.learner.error" = "warn"))
  warn_learner = makeFilterWrapper(warn_learner,
    fw.method = "FSelector_chi.squared")

  warn_learner = makeTuneWrapper(warn_learner,
    resampling = inner,
    control = ctrl,
    par.set = filter.ps, show.info = TRUE)

  # Tests
  # Expect benchmark failing
  suppressMessages(
    expect_error(benchmark(
      learners = stop.learner, tasks = task,
      resamplings = outer,
      keep.pred = FALSE, models = FALSE, show.info = TRUE))
  )

  # Expect benchmark warning
  suppressWarnings(suppressMessages(
    expect_warning(benchmark(
      learners = warn_learner, tasks = task,
      resamplings = outer,
      keep.pred = FALSE, models = FALSE, show.info = TRUE))
  ))

  # Expect benchmark messages
  suppressMessages(
    expect_message({
      bmr = benchmark(
        learners = quiet_learner, tasks = task,
        resamplings = outer, keep.pred = FALSE, models = FALSE, show.info = TRUE)
    })
  )
  aggr_perf = getBMRAggrPerformances(bmr = bmr)

  # Check result
  expect_class(x = bmr, classes = "BenchmarkResult")
  expect_true(object = is.na(aggr_perf[[1]][[1]]))
})
