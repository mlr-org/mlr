context("generateThreshVsPerfData")

test_that("generateThreshVsPerfData", {
  ## single prediction
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, binaryclass.task)
  pred = predict(mod, binaryclass.task)
  pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(pvs$measures)))
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(pvs$measures)))
  ## plotThreshVsPerfGGVIS(pvs)

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))

  ## resample prediction
  rdesc = makeResampleDesc("CV", iters = 2L)
  r = resample(lrn, binaryclass.task, rdesc)
  pvs = generateThreshVsPerfData(r, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(pvs$measures)))
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(pvs$measures)))
  ## plotThreshVsPerfGGVIS(pvs)

  pvs = generateThreshVsPerfData(r, list(tpr, fpr), aggregate = FALSE)
  plotThreshVsPerf(pvs)
  ggsave(path)

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))

  pvs = generateThreshVsPerfData(r, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(rdesc$iters))

  ## benchmark result
  lrns = list(lrn, makeLearner("classif.lda", predict.type = "prob"))
  rdesc = makeResampleDesc("CV", iters = 2L)
  res = benchmark(lrns, binaryclass.task, rdesc, show.info = FALSE)
  pvs = generateThreshVsPerfData(res, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(pvs$measures)))
  #expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)) - 1, equals(length(unique(pvs$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)) - 1, equals(length(unique(pvs$data$learner))))
  ## plotThreshVsPerfGGVIS(pvs)

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))

  pvs = generateThreshVsPerfData(res, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)),
              #equals(rdesc$iters * length(unique(pvs$data$learner))))

  ## list of resample predictions
  rs = lapply(lrns, crossval, task = binaryclass.task, iters = 2L)
  names(rs) = c("a", "b")
  pvs = generateThreshVsPerfData(rs, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(pvs$measures)))
  #expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)) - 1, equals(length(unique(pvs$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, blue.line.xpath, ns.svg)) - 1, equals(length(unique(pvs$data$learner))))
  ## plotThreshVsPerfGGVIS(pvs)

  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))

  pvs = generateThreshVsPerfData(rs, list(tpr, fpr), aggregate = FALSE)
  plotROCCurves(pvs, list(fpr, tpr), diagonal = FALSE)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(length(unique(pvs$data$learner))))
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)),
              #equals(rdesc$iters * length(unique(pvs$data$learner))))

  ## test prediction obj with custom measure
  classes = levels(getTaskTargets(binaryclass.task))
  mcm = matrix(sample(0:3, size = (length(classes))^2, TRUE), ncol = length(classes))
  rownames(mcm) = colnames(mcm) = classes
  costs = makeCostMeasure(id = "asym.costs", name = "Asymmetric costs",
                          minimize = TRUE, costs = mcm, binaryclass.task, combine = mean)
  pvs.custom = generateThreshVsPerfData(pred, costs)
  plotThreshVsPerf(pvs.custom)
  ggsave(path)
  ## doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, lightgrey.xpath, ns.svg)), equals(length(pvs.custom$measures)))
  #expect_that(length(XML::getNodeSet(doc, black.line.xpath, ns.svg)), equals(length(unique(pvs.custom$data$learner))))

  ## plotThreshVsPerfGGVIS(pvs.custom)
})
