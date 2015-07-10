context("normalizeFeatures")

test_that("normalizeFeatures", {
  df = data.frame(x = c(0,-1,4,2,3), y = letters[1:5], target = letters[1:5])
  task = makeClassifTask(data = df, target = "target")
  expect_equal(df, getTaskData(normalizeFeatures(task, method="range", range=c(-1,4))))
  expect_equal(df, getTaskData(normalizeFeatures(task, method = "center", exclude="x")))
  x = getTaskData(normalizeFeatures(task, method = "standardize"))$x
  expect_equal(mean(x), 0)
  expect_equal(sd(x), 1)
  x = getTaskData(normalizeFeatures(task, method = "center"))$x
  expect_equal(mean(x), 0)

  norm.task = normalizeFeatures(multiclass.task, method="range")
  expect_equal(norm.task$task.desc, multiclass.task$task.desc)
  expect_equal(range(getTaskData(norm.task)$Sepal.Width), c(0,1))
})
