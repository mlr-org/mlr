context("removeConstantFactors")

test_that("removeConstantFactors", {
  df = data.frame(a = 1:10, b = c("a", rep("almostConstant", 9)), c = rep("constant", 10))
  
  res = removeConstantFactors(df)
  expect_true(ncol(res) == 2)
  expect_true(all(colnames(res) == c("a", "b")))
  
  res = removeConstantFactors(df, perc = 20)
  expect_true(is.data.frame(res))
  expect_true(ncol(res) == 1)
  expect_true(colnames(res) == "a")
   
  df = data.frame(a = factor(rep("constant", 10), levels = c("constand", "unobservedSecondLevel")))
  res = removeConstantFactors(df)
  expect_true(is.data.frame(res))
  expect_true(ncol(res) == 0)
})
