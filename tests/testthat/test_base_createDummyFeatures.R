context("createDummyFeatures")

test_that("createDummyFeatures", {
  df = data.frame(a = 1:5, b = letters[1:5], c = LETTERS[c(1, 1, 1, 2, 2)], stringsAsFactors = FALSE)
  expect_equal(df, createDummyFeatures(df))

  df$c = as.factor(df$c)
  df.d = createDummyFeatures(df)
  expect_equal(colnames(df.d), c("a", "b", "c.A", "c.B"))

  df.d = createDummyFeatures(df, method = "reference")
  expect_equal(colnames(df.d), c("a", "b", "c.B"))

  df$b = as.factor(df$b)
  df.bc = createDummyFeatures(df)
  expect_equal(colnames(df.bc), c("a", "b.a", "b.b", "b.c", "b.d", "b.e", "c.A", "c.B"))

  grid = createDummyFeatures(expand.grid(x1 = letters[1:2], x2 = letters[3:4]), method = "reference")
  expect_equal(colnames(grid), c("x1.b", "x2.d"))

  dummy.task = createDummyFeatures(iris.task)
  expect_equal(dummy.task, iris.task)

  df$a = as.factor(df$a)
  expect_equal(c("a", "b", "c.A", "c.B"),
    colnames(createDummyFeatures(df, cols = "c")))

  df = data.frame(quan = as.factor(sample(0:1, 10, replace = TRUE)))
  levels(df$quan) = c("<5", ">5")
  df.cdf = createDummyFeatures(df)
  colnames = names(df.cdf)
  expect_false("<5" %in% colnames || ">5" %in% colnames)
  expect_true("quan..5" %in% colnames || "quan..5.1" %in% colnames)
})
