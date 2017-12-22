context("generateCalibration")

test_that("generateCalibrationData", {
  ## single prediction
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, binaryclass.task)
  pred = predict(mod, binaryclass.task)
  cd = generateCalibrationData(pred)
  expect_that(cd$proportion, is_a("data.frame"))
  expect_that(cd$data, is_a("data.frame"))
  plotCalibration(cd)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  ## resample prediction
  rdesc = makeResampleDesc("CV", iters = 2L)
  r = resample(lrn, binaryclass.task, rdesc)
  cd = generateCalibrationData(r)
  expect_that(cd$proportion, is_a("data.frame"))
  expect_that(cd$data, is_a("data.frame"))
  plotCalibration(cd)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  ## benchmark result
  lrns = list(lrn, makeLearner("classif.lda", predict.type = "prob"))
  res = benchmark(lrns, binaryclass.task, rdesc, show.info = FALSE)
  cd = generateCalibrationData(res)
  plotCalibration(cd)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  ## list of resample predictions
  rs = lapply(lrns, crossval, task = binaryclass.task, iters = 2L)
  names(rs) = c("a", "b")
  cd = generateCalibrationData(rs)
  plotCalibration(cd)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(length(unique(cd$data$Learner))))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)), equals(nrow(cd$proportion) + 1))

  # facetting works:
  q = q = plotCalibration(cd, facet.wrap.nrow = 2L)
  testFacetting(q, nrow = 2L)
  q = q = plotCalibration(cd, facet.wrap.ncol = 2L)
  testFacetting(q, ncol = 2L)
})
