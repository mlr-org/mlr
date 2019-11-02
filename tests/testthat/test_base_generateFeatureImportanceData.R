test_that("generateFeatureImportanceData", {
  regr.imp = generateFeatureImportanceData(regr.task, "permutation.importance",
    "regr.rpart", c("lstat", "crim"), FALSE, mse,
    function(x, y) abs(x - y), median, 1L, TRUE, FALSE)
  expect_that(colnames(regr.imp$res), equals(c("lstat", "crim")))
  expect_that(dim(regr.imp$res), equals(c(1, 2)))

  classif.imp = generateFeatureImportanceData(multiclass.task, "permutation.importance",
    "classif.rpart", c("Petal.Width", "Petal.Length"), TRUE, ber, nmc = 1L, local = TRUE)
  expect_that(colnames(classif.imp$res), equals(stri_paste("Petal.Width", "Petal.Length", sep = ":")))
  expect_that(dim(classif.imp$res), equals(c(getTaskSize(multiclass.task), 1)))

  # Test printer
  expect_output(print(classif.imp), regexp = "FeatureImportance:")
})
