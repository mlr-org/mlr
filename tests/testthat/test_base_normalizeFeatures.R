context("normalizeFeatures")

test_that("normalizeFeatures", {
  df = data.frame(x1 = c(0, -1, 4, 2, 3), x2 = letters[1:5],
    target = letters[1:5])
  task = makeClassifTask(data = df, target = "target")

  normalized = normalizeFeatures(df, method = "range", range = c(-1, 4))
  expect_equal(normalized, df)

  normalized = normalizeFeatures(task, method = "range", range = c(-1, 4))
  expect_equal(normalized, task)

  normed.x = getTaskData(normalizeFeatures(task, method = "center"))$x1
  expect_equal(mean(normed.x), 0)

  normed.task = normalizeFeatures(multiclass.task, method = "range")
  expect_equal(normed.task$task.desc, multiclass.task$task.desc)
  expect_equal(range(getTaskData(normed.task)$Sepal.Width), c(0, 1))

  err.message = "Assertion on 'cols' failed: Must be a subset of {'x1'}"

  expect_error(normalizeFeatures(df, cols = "x2"), err.message, fixed = TRUE)

  expect_error(normalizeFeatures(df, cols = "target"), err.message, fixed = TRUE)

  expect_error(normalizeFeatures(df, cols = "x100"), err.message, fixed = TRUE)
})
