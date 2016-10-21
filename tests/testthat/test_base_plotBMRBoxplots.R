context("plotBMRBoxplots")

test_that("BenchmarkResult", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  plotBMRBoxplots(res)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
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
})

test_that("BenchmarkResult allows spaces", {
  cv = makeResampleDesc("CV", iters = 2L)
  measures = list(mlr::auc)
  learners = list(
    makeLearner("classif.rpart", predict.type = "prob")
  )
  res = benchmark(learners, sonar.task, cv, measures)
  plotBMRBoxplots(res, measure=auc)
  ggsave(tempfile(fileext = ".png"))
})
