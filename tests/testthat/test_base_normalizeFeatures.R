context("normalizeFeatures")

test_that("normalizeFeatures", {
  df = data.frame(x = c(0,-1,4,2,3), y = letters[1:5])
  expect_equal(df, normalizeFeatures(df, method="range", range=c(-1,4)))
  expect_equal(df, normalizeFeatures(df, method = "center", exclude="x"))
  x = normalizeFeatures(df, method = "standardize")$x
  expect_equal(mean(x), 0)
  expect_equal(sd(x), 1)
  x = normalizeFeatures(df, method = "center")$x
  expect_equal(mean(x), 0)

  norm.task = normalizeFeatures(multiclass.task, method="range")
  expect_equal(norm.task$task.desc, multiclass.task$task.desc)
  expect_equal(range(norm.task$env$data$Sepal.Width), c(0,1))
})


