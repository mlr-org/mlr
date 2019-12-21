context("generateLearningCurve")

test_that("generateLearningCurve", {
  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
    task = binaryclass.task, percs = c(0.1, 0.3),
    measures = list(acc, timeboth))
  expect_true(all(c("learner", "percentage", "acc", "timeboth") %in% colnames(r$data)))

  plotLearningCurve(r)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)),
    equals(length(r$measures)))
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))
  expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))

  r = generateLearningCurveData(learners = list("regr.lm", "regr.svm"),
    task = regr.num.task, percs = c(0.1, 0.2),
    resampling = makeResampleDesc(method = "CV", iters = 2),
    measures = list(sse, timeboth))
  expect_true(all(c("learner", "percentage", "sse", "timeboth") %in% colnames(r$data)))

  plotLearningCurve(r)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)),
    equals(length(r$measures)))
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))
  expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))

  r = generateLearningCurveData(list("classif.rpart", "classif.knn"),
    task = binaryclass.task, percs = c(0.1, 0.3),
    resampling = makeResampleDesc("Holdout", predict = "both"),
    measures = list(acc, setAggregation(acc, train.mean)))
  plotLearningCurve(r)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)),
    equals(length(r$measures)))
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))
  expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)),
    equals(length(unique(r$data$learner))))

  # facetting works for plotLearningCurveData

  q = plotLearningCurve(r, facet.wrap.nrow = 2L)
  testFacetting(q, nrow = 2L)
  q = plotLearningCurve(r, facet.wrap.ncol = 2L, facet = "learner")
  testFacetting(q, ncol = 2L)
})

test_that("generateLearningCurve works if single learner is passed (not wrapped in list)", {
  # see issue #1046
  r = generateLearningCurveData(makeLearner("classif.rpart"), task = binaryclass.task,
    percs = c(0.1, 0.7), measures = list(acc, timeboth))
  expect_true(all(c("learner", "percentage", "acc", "timeboth") %in% colnames(r$data)))
})
