context("removeConstantFeatures")

test_that("removeConstantFeatures", {

  data = data.frame(
    a = c(1L, 2L),
    b = as.factor(1:2),
    c = c("a", "b"),
    d = as.factor(c(TRUE, FALSE)),
    e = c(NA, 1),
    f = c(1, NA),
    g = c(1, 1),
    n = c(0, 1 - 0.7 - 0.3),
    target = as.factor(1:2)
  )
  data = data[c(rep(1, 9), 2),]
  data$safe = seq_row(data)
  task = makeClassifTask("test", data = data, target = "target")

  res = getTaskData(removeConstantFeatures(task, perc = 0.1, dont.rm = "g"))
  expect_equal(colnames(res), c("g", "target", "safe"))

  res = getTaskData(removeConstantFeatures(task, na.ignore = TRUE))
  expect_equal(colnames(res), c("a", "b", "c", "d", "target", "safe"))

  res = getTaskData(removeConstantFeatures(task, tol = 0, na.ignore = TRUE))
  expect_true(setequal(colnames(res), c("a", "b", "c", "d", "target", "safe", "n")))

  res = getTaskData(removeConstantFeatures(task, na.ignore = FALSE, perc = 0.2))
  expect_equal(colnames(res), c("target", "safe"))
})
