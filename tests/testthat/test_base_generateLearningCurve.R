context("generateLearningCurve")

test_that("generateLearningCurve", {
  r1 = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                task = sonar.task, percs = c(0.1, 0.3),
                                measures = list(acc, timeboth))
  expect_true(all(c("learner", "perc", "acc", "timeboth") %in% colnames(r1)))
  plotLearningCurve(r1)

  r2 = generateLearningCurveData(learners = list("regr.lm", "regr.svm"),
                                task = regr.num.task, percs = c(0.1, 0.2),
                                resampling = makeResampleDesc(method = "CV", iters = 2),
                                measures = list(sse, timeboth))
  expect_true(all(c("learner", "perc", "sse", "timeboth") %in% colnames(r2)))
  plotLearningCurve(r2)

  r3 = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                 task = sonar.task, percs = c(0.1, 0.3),
                                 measures = acc)
  plotLearningCurveGGVIS(r3)
})
