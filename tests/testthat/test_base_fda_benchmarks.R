test_that("benchmarking on fda tasks works", {

  lrns = list(makeLearner("classif.fdaknn"), makeLearner("classif.rpart"))
  expect_equal(sapply(lrns, function(x) class(x)[1]), c("classif.fdaknn", "classif.rpart"))
  expect_equal(sapply(lrns, function(x) class(x)[2]), c("RLearnerClassif", "RLearnerClassif"))
  expect_message({bmr = benchmark(lrns, fda.binary.gp.task.small, cv2)},  "Functional features have been")
  expect_class(bmr, "BenchmarkResult")
  expect_equal(names(bmr$results$gp.fdf), c("classif.fdaknn", "classif.rpart"))
  expect_numeric(as.data.frame(bmr)$mmce, lower = 0L, upper = 1L)


  # FIXME: Should work when Xudong finished FDboost learner
  lrns2 = list(makeLearner("regr.fdaFDboost"), makeLearner("regr.rpart"))
  expect_equal(sapply(lrns2, function(x) class(x)[1]), c("regr.fdaFDboost", "regr.rpart"))
  expect_equal(sapply(lrns2, function(x) class(x)[2]), c("RLearnerRegr", "RLearnerRegr"))
  # expect_message({bmr2 = benchmark(lrns2, fda.regr.fs.task, cv2)},  "Functional features have been")
  # expect_class(bmr2, "BenchmarkResult")
  # expect_equal(names(bmr2$results$fsFdf), c("regr.fdaFDboost", "regr.rpart"))
  # expect_numeric(as.data.frame(bmr2)$mse, lower = 0L, upper = Inf)

})
