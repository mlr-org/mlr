context("plotRankMatrixAsBar")

test_that("RankMatrix", {
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
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerShortNames(res))

  plotBMRRanksAsBarChart(res, pretty.names = FALSE)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  testDocForStrings(doc, getBMRLearnerIds(res))
})
