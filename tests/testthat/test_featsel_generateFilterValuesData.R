# these are "simple" tests for generateFilterValuesData and filterFeatures.
# we test the general code here, for exhaustive tests for all filters please look at file
# "test_featsel_filters.R"

context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  f = filterFeatures(binaryclass.task, method = "variance", select = "threshold",
    threshold = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp.old = suppressWarnings(generateFilterValuesData(binaryclass.task))
  expect_data_frame(feat.imp.old$data,
    types = c("character", "numeric", "factor"),
    nrows = length(ns), ncols = 4,
    col.names = "named")
  expect_equal(ns, feat.imp.old$data$name)

  feat.imp.new = suppressWarnings(generateFilterValuesData(binaryclass.task))
  expect_data_frame(feat.imp.new$data,
    types = c("character", "numeric", "factor"),
    nrows = length(ns), ncols = 4,
    col.names = "named")
  expect_equal(names(feat.imp.new$data), c("name", "type", "filter", "value"))
  expect_equal(ns, feat.imp.new$data$name)

  feat.imp.old = suppressWarnings(generateFilterValuesData(binaryclass.task,
    method = "variance"))
  expect_data_frame(feat.imp.old$data,
    types = c("character", "numeric", "factor"),
    nrows = length(ns), ncols = 4,
    col.names = "named")
  expect_equal(ns, feat.imp.old$data$name)

  f = filterFeatures(binaryclass.task, method = "variance", abs = 5L)
  expect_true(setequal(getTaskFeatureNames(f), head(sortByCol(feat.imp.old$data,
    "value", asc = FALSE), 5L)$name))

  # now check that we get the same result by operating on
  # generateFilterValuesData
  feat.imp.old = suppressWarnings(generateFilterValuesData(binaryclass.task,
    method = "variance"))
  ff = filterFeatures(binaryclass.task, fval = feat.imp.old, abs = 5L)
  expect_equal(f, ff)

  feat.imp.new = generateFilterValuesData(binaryclass.task, method = "variance")
  expect_data_frame(feat.imp.new$data, types = c("character", "numeric", "factor"),
    nrow = length(ns), ncols = 4, col.names = "named")
  expect_equal(names(feat.imp.new$data), c("name", "type", "filter", "value"))
  expect_equal(ns, feat.imp.new$data$name)

  f = filterFeatures(binaryclass.task, method = "variance", abs = 5L)
  expect_true(setequal(getTaskFeatureNames(f),
    head(sortByCol(feat.imp.new$data, "value", asc = FALSE), 5L)$name))

  # now check that we get the same result by operating on generateFilterValuesData
  feat.imp.new = generateFilterValuesData(binaryclass.task, method = "variance")
  ff = filterFeatures(binaryclass.task, fval = feat.imp.new, abs = 5L)
  expect_equal(f, ff)

  f1 = filterFeatures(binaryclass.task, abs = 1L, mandatory.feat = "V1",
    ntree = 1L)
  f2 = subsetTask(binaryclass.task, features = "V1")
  expect_equal(f1, f2)

  f1 = filterFeatures(multiclass.task, abs = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(multiclass.task, perc = 0.5)
  expect_equal(f1, f2)

  lrn1 = makeFilterWrapper("classif.rpart", fw.perc = 0.2)
  m = train(lrn1, multiclass.task)
  f = getFilteredFeatures(m)
  expect_is(f, "character")
  expect_equal(length(f), 1L)
})

test_that("args are passed down to filter methods", {
  # we had an issue here, see #941

  expect_error(generateFilterValuesData(regr.num.task,
    method = c("mrmr", "univariate.model.score"),
    nselect = 3, perf.learner = "regr.lm"), "Please pass extra arguments")

  # check that we can pass down perf.learner to univariate.model.score, and get
  # no error from mrmr call
  f = generateFilterValuesData(regr.num.task,
    method = c("mrmr", "univariate.model.score"),
    nselect = 3,
    more.args = list(univariate.model.score = list(perf.learner = "regr.lm")))

  # create stupid dummy data and check that we can change the na.rm arg of
  # filter "variance" in multiple ways
  d = iris
  d[1L, 1L] = NA_real_
  task = makeClassifTask(data = d, target = "Species")

  f1 = generateFilterValuesData(task, method = "variance", na.rm = FALSE)
  f2 = generateFilterValuesData(task, method = "variance", na.rm = TRUE)
  f3 = generateFilterValuesData(task, method = "variance",
    more.args = list(variance = list(na.rm = TRUE)))
  f4 = generateFilterValuesData(task,
    method = c("univariate.model.score", "variance"),
    more.args = list(variance = list(na.rm = TRUE)))

  expect_true(is.na(f1$data$value[1L]))
  expect_false(is.na(f2$data$value[1L]))
  expect_false(is.na(f3$data$value[1L]))
  expect_false(is.na(f4$data$value[1L]))
})

test_that("errors for unsupported task and feature types", {
  expect_error(generateFilterValuesData(multiclass.task,
    method = c("mrmr", "variance", "linear.correlation")),
  "Filter(s) 'mrmr', 'linear.correlation' not compatible with task of type 'classif'",
  fixed = TRUE)
  expect_error(generateFilterValuesData(regr.task,
    method = c("mrmr", "carscore")),
  "Filter(s) 'mrmr', 'carscore' not compatible with features of type 'factors', and 'factors' respectively",
  fixed = TRUE)
  expect_error(generateFilterValuesData(regr.task,
    method = "carscore"),
  "Filter(s) 'carscore' not compatible with features of type 'factors' respectively",
  fixed = TRUE)
})

test_that("filter values are named and ordered correctly", {
  # we had an issue here, see #940

  ns = getTaskFeatureNames(regr.task)
  mock.filter = makeFilter(
    "mock.filter",
    desc = "Mock Filter",
    pkg = character(0),

    supported.tasks = c("classif", "regr", "surv"),
    supported.features = c("numerics", "factors"),
    fun = function(task, nselect) {
      ns = getTaskFeatureNames(task)
      d = seq_along(ns)
      names(d) = ns
      d = c(d[-1], d[1])
      d
    })
  fv = generateFilterValuesData(regr.task, method = "mock.filter")
  expect_equal(fv$data$name, ns)
  expect_equal(fv$data$value, seq_along(ns))
  rm("mock.filter", envir = mlr:::.FilterRegister)
})

test_that("filter method 'variance' works with missing values", {
  fi = generateFilterValuesData(regr.na.num.task, method = "variance")
  expect_false(anyMissing(fi$data$value))
})

test_that("ensemble methods work", {
  fi = generateFilterValuesData(multiclass.task,
    method = list("E-min", c("FSelectorRcpp_gain.ratio",
      "FSelectorRcpp_information.gain")))
  expect_true(all(!is.na(fi$data$value) == TRUE))
})
