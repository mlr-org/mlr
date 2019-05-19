context("plotBMRSummary")

test_that("BenchmarkSummary", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, timeboth)
  res = benchmark(lrns, tasks, rdesc, meas)
  n.tasks = length(getBMRTaskIds(res))
  n.lrns = length(getBMRLearnerIds(res))

  plotBMRSummary(res)

  # pretty.names works
  plotBMRSummary(res)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res))

  plotBMRSummary(res, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerIds(res))

  # check error when learner short names are not unique
  lrns = list(
    rf = makeLearner("classif.randomForest", id = "rf1"),
    rf2 = makeLearner("classif.randomForest", id = "rf2")
  )
  res = benchmark(lrns, tasks, rdesc, meas)
  expect_error(plotBMRSummary(res),
    "names are not unique")
})
