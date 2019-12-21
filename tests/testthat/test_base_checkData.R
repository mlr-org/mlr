context("checkData")

test_that("checkData", {
  expect_error(
    makeClassifTask(data = binaryclass.df, target = "foo"),
    "doesn't contain target var: foo"
  )

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

  # check costiris.task has costs
  expect_equal(nrow(getTaskData(costiris.task)), nrow(getTaskCosts(costiris.task)))
})

test_that("changeData . getTaskData is a noop on builtin tasks", {
  # We expect changeData(task, getTaskData(task, ...)) to not change task.
  # If it does, it means that the internal format of task or task.desc has
  # changed, and that the data needs to be re-generated.
  pkgdata = data(package = "mlr")$results[, "Item"]
  tasknames = grep("\\.task$", pkgdata, value = TRUE)
  for (task in tasknames) {
    taskdata = get(task)
    changeddata = changeData(taskdata, getTaskData(taskdata, functionals.as = "matrix"))
    expect_equal(taskdata, changeddata)
  }
})
