context("selectFeatures")

test_that("selectFeatures", {
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.lda")

  ### check all methods, with restricted number of iterations

  ctrl = makeFeatSelControlExhaustive(max.features = 1)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_equal(getOptPathLength(fr$opt.path), 5)

  ctrl = makeFeatSelControlRandom(maxit = 2)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_equal(getOptPathLength(fr$opt.path), 2)

  ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0.01)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1)

  ctrl = makeFeatSelControlSequential(method = "sbs", beta = 0.01)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1)

  ctrl = makeFeatSelControlSequential(method = "sffs", alpha = 0.01)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  # we must at least try to select a 2nd feature
  expect_true(getOptPathLength(fr$opt.path) >= 1 + 4 + 1 + 3)

  ctrl = makeFeatSelControlSequential(method = "sfbs", beta = 0.01)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_true(getOptPathLength(fr$opt.path) > 1)

  ### check max.features setting for all methods
  # reduce features so it does not take ages to test
  mytask = subsetTask(binaryclass.task,
    features = getTaskFeatureNames(binaryclass.task)[1:5])

  ctrl = makeFeatSelControlSequential(alpha = 0, max.features = 1L, method = "sfs")
  fr = selectFeatures(lrn, task = mytask, resampling = rdesc, control = ctrl,
    show.info = FALSE)
  expect_equal(length(fr$x), 1)

  ctrl = makeFeatSelControlSequential(beta = 1, max.features = 3L, method = "sbs")
  fr = selectFeatures(lrn, task = mytask, resampling = rdesc, control = ctrl,
    show.info = FALSE)
  expect_equal(length(fr$x), 3L)

  ctrl = makeFeatSelControlGA(maxit = 5L, max.features = 2L)
  fr = selectFeatures(lrn, task = mytask, resampling = rdesc, control = ctrl,
    show.info = FALSE)
  expect_true(length(fr$x) <= 2L)

  ctrl = makeFeatSelControlRandom(maxit = 6L, max.features = 1L)
  fr = selectFeatures(lrn, task = mytask, resampling = rdesc, control = ctrl,
    show.info = FALSE)
  expect_true(length(fr$x) <= 1)

  ### check bits.to.features setting for all methods

  bns = c("b1", "b2")
  btf = function(x, task) {
    fns = getTaskFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(x)], init = character(0))
  }

  ctrl = makeFeatSelControlRandom(maxit = 3)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob",
    "eol", "exec.time", "error.message")))
  expect_equal(nrow(df), 3)

  ctrl = makeFeatSelControlExhaustive()
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob",
    "eol", "exec.time", "error.message")))
  expect_equal(nrow(df), 4)

  ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob",
    "eol", "exec.time", "error.message")))
  expect_equal(nrow(df), 4)

  ctrl = makeFeatSelControlGA(maxit = 5, lambda = 6, mu = 15)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob",
    "eol", "exec.time", "error.message")))
  expect_equal(nrow(df), 15 + 5 * 6)
})
