context("plotBMRRanksAsBarChart")

test_that("plotBMRRanksAsBarChart", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  rdesc = makeResampleDesc("CV", iters = 2L)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  n.tsks = length(getBMRTaskIds(res))
  n.lrns = length(getBMRLearnerIds(res))

  plotBMRRanksAsBarChart(res)
  plotBMRRanksAsBarChart(res, pos = "stack")
  plotBMRRanksAsBarChart(res, pos = "dodge")
  # pretty.names works
  plotBMRRanksAsBarChart(res)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res))

  plotBMRRanksAsBarChart(res, pretty.names = FALSE)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerIds(res))

  # test pretty.names in conjunction with order.lrns
  new.order = c("classif.rpart", "classif.nnet")
  plotBMRRanksAsBarChart(res, pretty.names = TRUE, order.lrns = new.order)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res)[2:1], ordered = TRUE)

  # check error when learner short names are not unique
  lrns = list(
    rf = makeLearner("classif.randomForest", id = "rf1"),
    rf2 = makeLearner("classif.randomForest", id = "rf2")
  )
  res = benchmark(lrns, tasks)
  expect_error(plotBMRSummary(res),
    "names are not unique")
})
