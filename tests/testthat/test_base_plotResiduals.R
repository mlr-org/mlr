context("plotResiduals")

test_that("plotResiduals with prediction object", {
  learner = makeLearner("regr.rpart")
  mod = train(learner, regr.task)
  preds = predict(mod, regr.task)
  plotResiduals(preds)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  # points
  expect_equal(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), getTaskSize(regr.task))
  # loess
  expect_equal(length(XML::getNodeSet(doc, mediumblue.line.xpath, ns.svg)), 1L)
  # rug
  expect_equal(length(XML::getNodeSet(doc, red.rug.line.xpath, ns.svg)), getTaskSize(regr.task) * 2L)

  # histogram
  plotResiduals(preds, type = "hist")
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.bar.xpath, ns.svg)), 30L)
  # task.type == "classif"
  learner = makeLearner("classif.rpart")
  mod = train(learner, multiclass.task)
  preds = predict(mod, multiclass.task)
  plotResiduals(preds)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  num.points = sum(calculateConfusionMatrix(preds)$result[1:3, 1:3] != 0)
  expect_true(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)) > num.points)
})

test_that("plotResiduals with BenchmarkResult", {
  lrns = list(makeLearner("classif.ksvm"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  bmr = benchmark(lrns, tasks, hout, measures = getDefaultMeasure(multiclass.task))
  plotResiduals(bmr, type = "scatterplot")
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  grid.size = length(getBMRTaskIds(bmr)) * length(getBMRLearnerIds(bmr))
  # facets
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), grid.size)
  # histogram
  plotResiduals(bmr, type = "hist")
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  # barplot now. We can't test for exact number of bars anymore
  expect_true(length(XML::getNodeSet(doc, black.bar.xpath, ns.svg)) > 0L)

  # check pretty names
  testDocForStrings(doc, getBMRLearnerShortNames(bmr), grid.size = 2L)

  plotResiduals(bmr, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(bmr), grid.size = 2L)

  # check error when learner short names are not unique
  lrns = list(
    rf = makeLearner("classif.randomForest", id = "rf1"),
    rf2 = makeLearner("classif.randomForest", id = "rf2")
  )
  res = benchmark(lrns, tasks, hout)
  expect_error(plotBMRSummary(res),
    "names are not unique")
})
