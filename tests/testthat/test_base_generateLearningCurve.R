context("generateLearningCurve")

test_that("generateLearningCurve", {
  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                task = binaryclass.task, percs = c(0.1, 0.3),
                                measures = list(acc, timeboth))
  expect_true(all(c("learner", "perc", "acc", "timeboth") %in% colnames(r$data)))
  plotLearningCurve(r)
  ## plotLearningCurveGGVIS(r)

  r = generateLearningCurveData(learners = list("regr.lm", "regr.svm"),
                                task = regr.num.task, percs = c(0.1, 0.2),
                                resampling = makeResampleDesc(method = "CV", iters = 2),
                                measures = list(sse, timeboth))
  expect_true(all(c("learner", "perc", "sse", "timeboth") %in% colnames(r$data)))
  plotLearningCurve(r)
  ## plotLearningCurveGGVIS(r)

  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                task = binaryclass.task, percs = c(0.1, 0.3),
                                measures = acc)
  plotLearningCurve(r)
  plotLearningCurveGGVIS(r) ## not interactive by default
})
