context("createDummyFeatures")

test_that("createDummyFeatures", {
  df = data.frame(a = 1:5, b = letters[1:5], c = LETTERS[c(1,1,1,2,2)], stringsAsFactors = FALSE)
  expect_equal(df, createDummyFeatures(df))
  df$c = as.factor(df$c)
  df.d = createDummyFeatures(df)
  expect_equal(colnames(df.d), c("a", "b", "c.A", "c.B"))
  df.d = createDummyFeatures(df, method = "reference")
  expect_equal(colnames(df.d), c("a", "b", "c.B"))
  
  dummy.task = createDummyFeatures(iris.task)
  expect_equal(dummy.task, iris.task)
})


