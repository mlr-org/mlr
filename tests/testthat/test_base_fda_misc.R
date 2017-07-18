context("fda_benchmarking_and_mixed_learners")
test_that("benchmarking on fda tasks works", {

  lrns = list(makeLearner("classif.fdaknn"), makeLearner("classif.rpart"))
  expect_equal(vcapply(lrns, function(x) class(x)[1]), c("classif.fdaknn", "classif.rpart"))
  expect_equal(vcapply(lrns, function(x) class(x)[2]), c("RLearnerClassif", "RLearnerClassif"))
  expect_message({bmr = benchmark(lrns, fda.binary.gp.task.small, cv2)},  "Functional features have been")
  expect_class(bmr, "BenchmarkResult")
  expect_equal(names(bmr$results$gp.fdf), c("classif.fdaknn", "classif.rpart"))
  expect_numeric(as.data.frame(bmr)$mmce, lower = 0L, upper = 1L)


  # FIXME: Should work when Xudong finished FDboost learner
  lrns2 = list(makeLearner("regr.fdaFDboost"), makeLearner("regr.rpart"))
  expect_equal(vcapply(lrns2, function(x) class(x)[1]), c("regr.fdaFDboost", "regr.rpart"))
  expect_equal(vcapply(lrns2, function(x) class(x)[2]), c("RLearnerRegr", "RLearnerRegr"))
  # expect_message({bmr2 = benchmark(lrns2, fda.regr.fs.task, cv2)},  "Functional features have been")
  # expect_class(bmr2, "BenchmarkResult")
  # expect_equal(names(bmr2$results$fsFdf), c("regr.fdaFDboost", "regr.rpart"))
  # expect_numeric(as.data.frame(bmr2)$mse, lower = 0L, upper = Inf)
})


test_that("benchmarking on fda tasks works", {
  expect_error(train(makeLearner("classif.fdaknn"), iris.task), "numeric inputs")
})



test_that("FDA properties work", {
  df = data.frame(matrix(rnorm(100), nrow = 10), "target" = sample(letters[1:2], 10, replace = TRUE))
  # Transform to functional data (fd.features indicate column names or indices)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:6, "fd2" = 8:10))
  # Create a classification task
  tsk = makeClassifTask(data = fdf, target = "target")
  lrn = makeLearner("classif.fdaknn")
  # Error for multiple functional inputs
  expect_error(resample(lrn, subsetTask(tsk, features = 2:3), cv2), "more than one functional inputs")
  # Error for numeric inputs
  expect_error(train(lrn, subsetTask(tsk, features = 1:3)), "numeric inputs")
  expect_error(train(lrn, subsetTask(tsk, features = 1)), "numeric inputs")
  # No error for single functional
  expect_silent(train(lrn, subsetTask(tsk, features = 2)))
  expect_silent(train(lrn, subsetTask(tsk, features = 3)))
})
