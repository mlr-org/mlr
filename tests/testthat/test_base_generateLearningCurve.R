context("generateLearningCurve")

test_that("generateLearningCurve", {
  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                task = binaryclass.task, percs = c(0.1, 0.3),
                                measures = list(acc, timeboth))
  expect_true(all(c("learner", "percentage", "acc", "timeboth") %in% colnames(r$data)))
  plotLearningCurve(r)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(r$measures)))
  #expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  ## plotLearningCurveGGVIS(r)

  r = generateLearningCurveData(learners = list("regr.lm", "regr.svm"),
                                task = regr.num.task, percs = c(0.1, 0.2),
                                resampling = makeResampleDesc(method = "CV", iters = 2),
                                measures = list(sse, timeboth))
  expect_true(all(c("learner", "percentage", "sse", "timeboth") %in% colnames(r$data)))
  plotLearningCurve(r)
  ggsave(path)
  doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(r$measures)))
  #expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  ## plotLearningCurveGGVIS(r)

  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
                                task = binaryclass.task, percs = c(0.1, 0.3),
                                resampling = makeResampleDesc("Holdout", predict = "both"),
                                measures = list(acc, setAggregation(acc, train.mean)))
  plotLearningCurve(r)
  ggsave(path)
  doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(r$measures)))
  #expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)) - 1, equals(length(unique(r$data$learner))))
  ## plotLearningCurveGGVIS(r)
})
