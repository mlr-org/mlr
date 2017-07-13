context("checkData")

test_that("checkData", {
  expect_error(makeClassifTask(data = binaryclass.df, target = "foo"), "doesn't contain target var: foo")

  # y contains missings
  df = multiclass.df
  df[1, multiclass.target] = NA
  expect_error(makeClassifTask(data = df, target = multiclass.target), "missing values")
  df = regr.df
  df[1, regr.target] = NaN
  expect_error(makeRegrTask(data = df, target = regr.target), "missing values")

  # data contains infs
  df = regr.df
  df[1, regr.target] = Inf
  expect_error(makeRegrTask(data = df, target = regr.target), "be finite")
  df = regr.df
  df[1, getTaskFeatureNames(regr.task)[1]] = Inf
  expect_error(makeRegrTask(data = df, target = regr.target), "infinite")

  # data contains nans
  df = regr.df
  df[1, getTaskFeatureNames(regr.task)[1]] = NaN
  expect_error(makeRegrTask(data = df, target = regr.target), "contains NaN")

  # check conversion of target
  df = binaryclass.df
  df[, binaryclass.target] = as.character(df[, binaryclass.target])
  task = makeClassifTask(data = df, target = binaryclass.target)
  expect_true(is.factor(getTaskTargets(task)))

  df = binaryclass.df
  df[, binaryclass.target] = as.logical(as.integer(binaryclass.df[, binaryclass.target]) - 1)
  task = makeClassifTask(data = df, target = binaryclass.target)
  expect_true(is.factor(getTaskTargets(task)))

  df = regr.df
  df[, regr.target] = as.integer(regr.df[, regr.target])
  task = makeRegrTask(data = df, target = regr.target)
  expect_true(is.numeric(getTaskTargets(task)))

  # check unsupported columns
  df = multiclass.df
  df[, 1] = as.logical(df[, 1])
  colnames(df)[1] = "aaa"
  expect_error(makeClassifTask(data = df, target = multiclass.target), "Unsupported feature type")

  # check missing target column
  expect_error(makeOneClassTask(data = oneclass.df, positive = "FALSE", negative = "TRUE"), "argument \"target\" is missing, with no default")
  expect_error(makeOneClassTask(data = oneclass.df, target = "Anomaly", positive = "FALSE", negative = "TRUE"), "Assertion on 'Anomaly' failed: Must be of type 'factor', not 'NULL'.")
  expect_error(makeOneClassTask(data = oneclass.df, target = "V2", positive = "FALSE", negative = "TRUE"), "Assertion on 'V2' failed: Must be of type 'factor', not 'double'.")

  # if target column has two class levels: check if missing positive/negative input will return error
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", negative = "FALSE"), "argument \"positive\" is missing, with no default")
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", positive = "FALSE"), "argument \"negative\" is missing, with no default")
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal"), "argument \"positive\" is missing, with no default")

  # if target column has two class levels and positive or negative are wrongly named
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", positive = "Anomaly", negative = "TRUE"), "'positive' or 'negative' not equal to class levels")

  # data with target with one class level
  oneclass.df.true = oneclass.df[oneclass.df$normal == "TRUE", , drop = TRUE]
  oneclass.df.true$normal = droplevels(oneclass.df.true$normal)

  # if target column has one class level, check autoset of second class
  expect_set_equal(levels(makeOneClassTask(data = oneclass.df.true, target = "normal", positive = "TRUE", negative = "ANOMALY")$env$data$normal), c("TRUE", "ANOMALY"))
  expect_set_equal(levels(makeOneClassTask(data = oneclass.df.true, target = "normal", positive = "ANOMALY", negative = "TRUE")$env$data$normal), c("TRUE", "ANOMALY"))

  # if target column has one class level and positive and negative are wrongly named
  expect_error(makeOneClassTask(data = oneclass.df.true, target = "normal", positive = "NORMAL", negative = "ANOMALY"), "Neither 'positive' nor 'negative' are subset of class levels")
  })
