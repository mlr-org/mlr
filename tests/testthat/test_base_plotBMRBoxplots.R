context("plotBMRBoxplots")

test_that("BenchmarkResult", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  plotBMRBoxplots(res)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(getBMRTaskIds(res)))

  # facetting works:
  q = plotBMRBoxplots(res, facet.wrap.nrow = 2L)
  testFacetting(q, 2L)
  q = plotBMRBoxplots(res, facet.wrap.ncol = 2L)
  testFacetting(q, ncol = 2L)
  q = plotBMRBoxplots(res, facet.wrap.nrow = 2L, facet.wrap.ncol = 2L)
  testFacetting(q, 2L, 2L)

  # pretty names works
  plotBMRBoxplots(res)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res), grid.size = 2L)
  testDocForStrings(doc, getBMRMeasures(res)[[1L]]$name)

  plotBMRBoxplots(res, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerIds(res), grid.size = 2L)
  testDocForStrings(doc, getBMRMeasureIds(res)[[1L]])

  # test pretty.names in conjunction with order.lrns
  new.order = c("classif.rpart", "classif.nnet")
  plotBMRBoxplots(res, pretty.names = TRUE, order.lrns = new.order)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res)[2:1],
    grid.size = 2L, ordered = TRUE)

  # check error when learner short names are not unique
  lrns = list(
    rf = makeLearner("classif.randomForest", id = "rf1"),
    rf2 = makeLearner("classif.randomForest", id = "rf2")
  )
  res = benchmark(lrns, tasks, rdesc, meas)
  expect_error(plotBMRSummary(res),
    "names are not unique")
})

test_that("BenchmarkResult allows spaces", {
  cv = makeResampleDesc("CV", iters = 2L)
  measures = list(mlr::auc)
  learners = list(
    makeLearner("classif.rpart", predict.type = "prob")
  )
  res = benchmark(learners, sonar.task, cv, measures)
  plotBMRBoxplots(res, measure = auc)
  ggsave(tempfile(fileext = ".png"))
})
