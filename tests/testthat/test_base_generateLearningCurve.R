context("generateLearningCurve")

test_that("generateLearningCurveData", {
  r = generateLearningCurveData(list("classif.rpart", "classif.knn"), task = sonar.task, percs = c(0.1, 0.3), measures = list(acc, timeboth))
  expect_true(all(c("learner", "perc", "acc", "timeboth") %in% colnames(r)))

  r = generateLearningCurveData(learners = list("regr.lm", makeLearner("regr.svm")), task = regr.num.task, percs = c(0.1, 0.2), resampling = makeResampleDesc(method = "CV", iters = 2), measures = list(sse, timeboth))
  plotLearningCurve(r)
})
