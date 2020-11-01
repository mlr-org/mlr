
test_that("generateThreshVsPerfData", {

  ## single prediction
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, binaryclass.task)
  pred = predict(mod, binaryclass.task)
  pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  dir = tempdir()
  path = file.path(dir, "test.svg")
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(pvs$measures))
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), length(pvs$measures))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), 1L)

  ## resample prediction
  rdesc = makeResampleDesc("CV", iters = 2L)
  r = resample(lrn, binaryclass.task, rdesc)
  pvs = generateThreshVsPerfData(r, list(tpr, fpr))
  plotThreshVsPerf(pvs, pretty.names = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)),
               length(pvs$measures))
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)),
               length(pvs$measures))

  pvs = generateThreshVsPerfData(r, list(tpr, fpr, acc), aggregate = FALSE)
  plotThreshVsPerf(pvs, measures = list(tpr, fpr, acc))
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)),
              length(pvs$measures) * length(unique(pvs$data$iter)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)),
              length(unique(pvs$data$iter)))

  pvs = generateThreshVsPerfData(r, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), rdesc$iters)

  ## benchmark result
  lrns = list(lrn, makeLearner("classif.lda", predict.type = "prob"))
  rdesc = makeResampleDesc("CV", iters = 2L)
  res = benchmark(lrns, binaryclass.task, rdesc, show.info = FALSE,
    measures = getDefaultMeasure(binaryclass.task))
  pvs = generateThreshVsPerfData(res, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(pvs$measures))
  expect_equal(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), length(unique(pvs$data$learner)))
  expect_equal(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)), length(unique(pvs$data$learner)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = TRUE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), length(unique(pvs$data$learner)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), 1)
  expect_equal(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)), 1)

  pvs = generateThreshVsPerfData(res, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = TRUE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), rdesc$iters * length(unique(pvs$data$learner)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), rdesc$iters)
  expect_equal(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)), rdesc$iters)

  ## list of resample predictions
  rs = lapply(lrns, crossval, task = binaryclass.task, iters = 2L)
  names(rs) = c("a", "b")
  pvs = generateThreshVsPerfData(rs, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(pvs$measures))
  expect_equal(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), length(unique(pvs$data$learner)))
  expect_equal(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)), length(unique(pvs$data$learner)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = TRUE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(unique(pvs$data$learner)))
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), length(unique(pvs$data$learner)))

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = FALSE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)), 1)
  expect_equal(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), 1)

  pvs = generateThreshVsPerfData(rs, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE, facet.learner = TRUE)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), length(unique(pvs$data$learner)))
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), rdesc$iters * length(unique(pvs$data$learner)))

  ## test prediction obj with custom measure
  classes = levels(getTaskTargets(binaryclass.task))
  mcm = matrix(sample(0:3, size = (length(classes))^2, TRUE), ncol = length(classes))
  rownames(mcm) = colnames(mcm) = classes
  costs = makeCostMeasure(id = "asym.costs", name = "Asymmetric costs",
    minimize = TRUE, costs = mcm, combine = mean)
  pvs.custom = generateThreshVsPerfData(pred, costs)
  plotThreshVsPerf(pvs.custom)
  suppressMessages(ggsave(path))
  doc = XML::xmlParse(path)
  expect_equal(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), 1L)

  # test that facetting works for plotThreshVsPerf

  q = plotThreshVsPerf(pvs, facet.wrap.nrow = 2L)
  testFacetting(q, nrow = 2L)
  q = plotThreshVsPerf(pvs, facet.wrap.ncol = 2L)
  testFacetting(q, ncol = 2L)
})
