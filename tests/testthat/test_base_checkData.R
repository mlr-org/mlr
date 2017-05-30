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
  expect_error(makeOneClassTask(data = oneclass.df, positive = "TRUE", negative = "FALSE"), "argument \"target\" is missing, with no default")

  # if target column has two class levels: check if missing positive/negative input will correctly auto-set
  expect_equal(makeOneClassTask(data = oneclass.df, target = "normal", negative = "FALSE")$task.desc$positive, "TRUE")
  expect_equal(makeOneClassTask(data = oneclass.df, target = "normal", positive = "FALSE")$task.desc$negative, "TRUE")
  expect_set_equal(makeOneClassTask(data = oneclass.df, target = "normal")$task.desc$class.levels, c("TRUE", "FALSE"))

  # if target column has two class levels and positive or negative are wrongly named
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", positive = "Anomaly", negative = "FALSE"), "'positive' or 'negative' not equal to class levels")
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", positive = "Anomaly"), "'positive' not element of the two class levels")
  expect_error(makeOneClassTask(data = oneclass.df, target = "normal", negative = "Anomaly"), "'negative' not element of the two class levels")

  # if target column has one class levels
  oneclass.df.true = oneclass.df[oneclass.df$normal == "TRUE", , drop = TRUE]
  oneclass.df.true$normal = droplevels(oneclass.df.true$normal)
  expect_set_equal(levels(makeOneClassTask(data = oneclass.df.true, target = "normal", positive = "TRUE", negative = "FALSE")$env$data$normal), c("TRUE", "FALSE"))
  expect_error(makeOneClassTask(data = oneclass.df.true, target = "normal", negative = "TRUE"), "Cannot add second class level when 'negative' is equal to the only class level and no 'positive' is specified!")
  expect_set_equal(levels(makeOneClassTask(data = oneclass.df.true, target = "normal", negative = "FALSE")$env$data$normal), c("TRUE", "FALSE"))
  expect_set_equal(levels(makeOneClassTask(data = oneclass.df.true, target = "normal", positive = "TRUE", negative = "Anomaly")$env$data$normal), c("TRUE", "Anomaly"))
})
