context("fdaThreshVsPerf")

test_that("generateThreshVsPerfData for FDA Tasks", {
  ## single prediction on fda task
  lrn = makeLearner("classif.fdaknn", predict.type = "prob")
  mod = train(lrn, fda.binary.gp.task.small)
  pred = predict(mod, fda.binary.gp.task.small)
  pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggplot2::ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(length(pvs$measures)))
  expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(pvs$measures)))

})
