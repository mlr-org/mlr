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
  expect_equal(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), length(getBMRTaskIds(res)))
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
