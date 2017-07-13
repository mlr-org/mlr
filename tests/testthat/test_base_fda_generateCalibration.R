context("fdaGenerateCalibration")

test_that("generateCalibrationData works for fda lrn", {
  ## single prediction
  lrn = makeLearner("classif.fdaknn", predict.type = "prob")
  mod = train(lrn, fda.binary.gp.task.small)
  pred = predict(mod, fda.binary.gp.task.small)
  cd = generateCalibrationData(pred)
  expect_that(cd$proportion, is_a("data.frame"))
  expect_that(cd$data, is_a("data.frame"))
  plotCalibration(cd)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggplot2::ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  ## resample prediction
  rdesc = makeResampleDesc("CV", iters = 2L)
  r = resample(lrn, fda.binary.gp.task.small, rdesc)
  cd = generateCalibrationData(r)
  expect_that(cd$proportion, is_a("data.frame"))
  expect_that(cd$data, is_a("data.frame"))
  plotCalibration(cd)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggplot2::ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  # FIXME: to be added when second fdaclassif learner is added

  # ## benchmark result
  # lrns = list(lrn, makeLearner("fdaclassif.glm", predict.type = "prob"))
  # res = benchmark(lrns, fda.binary.gp.task.small, rdesc, show.info = FALSE)
  # cd = generateCalibrationData(res)
  # plotCalibration(cd)
  # ggsave(path)
  # doc = XML::xmlParse(path)
  # expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  # expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  # ## list of resample predictions
  # rs = lapply(lrns, crossval, task = binaryclass.task, iters = 2L)
  # names(rs) = c("a", "b")
  # cd = generateCalibrationData(rs)
  # plotCalibration(cd)
  # ggsave(path)
  # doc = XML::xmlParse(path)
  # expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  # expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))
  #
  # # facetting works:
  # q = q = plotCalibration(cd, facet.wrap.nrow = 2L)
  # testFacetting(q, 2L)
  # q = q = plotCalibration(cd, facet.wrap.ncol = 2L)
  # testFacetting(q, ncol = 2L)
})
