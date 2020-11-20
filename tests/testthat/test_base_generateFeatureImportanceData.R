test_that("generateFeatureImportanceData", {
  regr.imp = generateFeatureImportanceData(regr.task, "permutation.importance",
    "regr.rpart", c("lstat", "crim"), FALSE, mse,
    function(x, y) abs(x - y), median, 1L, TRUE, FALSE)
  expect_equal(colnames(regr.imp$res), c("lstat", "crim"))
  expect_equal(dim(regr.imp$res), c(1, 2))

  classif.imp = generateFeatureImportanceData(multiclass.task, "permutation.importance",
    "classif.rpart", c("Petal.Width", "Petal.Length"), TRUE, ber, nmc = 1L, local = TRUE)
  expect_equal(colnames(classif.imp$res), stri_paste("Petal.Width", "Petal.Length", sep = ":"))
  expect_equal(dim(classif.imp$res), c(getTaskSize(multiclass.task), 1))

  # Test printer
  expect_output(print(classif.imp), regexp = "FeatureImportance:")
})
