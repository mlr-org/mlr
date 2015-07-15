context("generatePartialPrediction")

test_that("generatePartialPredictionData", {
  fr = train("regr.rpart", regr.task)
  dr = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "chas"), TRUE,
                                     fmin = c(1, NA), fmax = c(40, NA), gridsize = 3L)
  nfeat = length(dr$features)
  n = nrow(dr$data)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(3L^2))

  plotPartialPrediction(dr, facet = "chas")
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(n))
  ## plotPartialPredictionGGVIS(dr, interaction = "chas")

  fc = train("classif.rpart", multiclass.task)
  dc = generatePartialPredictionData(fc, getTaskData(multiclass.task), c("Petal.Width", "Petal.Length"),
                                     fun = function(x) table(x) / length(x), gridsize = 3L)
  nfeat = length(dc$features)
  n = nrow(dc$data)
  plotPartialPrediction(dc)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(n))
  expect_that(length(XML::getNodeSet(doc, blue.xpath, "svg")) - 1, equals(n))
  expect_that(length(XML::getNodeSet(doc, green.xpath, "svg")) - 1, equals(n))
  ## plotPartialPredictionGGVIS(dc)

  fcp = train(makeLearner("classif.rpart", predict.type = "prob"), multiclass.task)
  expect_error(generatePartialPrediction(fcp, getTaskData(multiclass.task), "Petal.Width",
                                         function(x) quantile(x, c(.025, .5, .975))), gridsize = 3L)
  dcp = generatePartialPredictionData(fcp, getTaskData(iris.task), c("Petal.Width", "Petal.Length"),
                                      interaction = TRUE, gridsize = 3L)
  nfacet = length(unique(dcp$data$Petal.Length))
  ntarget = length(dcp$target)
  plotPartialPrediction(dcp, facet = "Petal.Length")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(ntarget * nfacet))
  expect_that(length(XML::getNodeSet(doc, blue.xpath, "svg")) - 1, equals(ntarget * nfacet))
  expect_that(length(XML::getNodeSet(doc, green.xpath, "svg")) - 1, equals(ntarget * nfacet))
  ## plotPartialPredictionGGVIS(dcp, interaction = "Petal.Length")

  fs = train("surv.coxph", surv.task)
  ds = generatePartialPredictionData(fs, getTaskData(surv.task), c("Petal.Width", "Petal.Length"), gridsize = 3L)
  nfeat = length(ds$features)
  n = nrow(ds$data)
  plotPartialPrediction(ds)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(n))
  ## plotPartialPredictionGGVIS(ds)

  db = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "chas"), TRUE,
                                     fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = 3L)
  nfeat = length(unique(db$data$chas))
  n = nrow(db$data)
  expect_that(colnames(db$data), equals(c("lower", "medv", "upper", "lstat", "chas")))
  plotPartialPrediction(db, facet = "chas")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(n))
  ## plotPartialPredictionGGVIS(db, interaction = "chas")

  db2 = generatePartialPredictionData(fr, getTaskData(regr.task), c("lstat", "crim"), FALSE,
                                      fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = 3L)
  nfeat = length(db2$features)
  n = nrow(db2$data)
  plotPartialPrediction(db2)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(n))
  ## plotPartialPredictionGGVIS(db2)

  fcpb = train(makeLearner("classif.rpart", predict.type = "prob"), binaryclass.task)
  bc = generatePartialPredictionData(fcpb, getTaskData(binaryclass.task), c("V11", "V12"))
  nfeat = length(bc$features)
  n = nrow(bc$data)
  plotPartialPrediction(bc)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(n))
  ## plotPartialPredictionGGVIS(bc)
})

test_that("generateFeatureGrid", {
  data = data.frame(
    w = seq(0, 1, length.out = 3),
    x = factor(letters[1:3]),
    y = ordered(1:3),
    z = 1:3
  )
  features = colnames(data)
  fmin = sapply(features, function(x) ifelse(!is.factor(data[[x]]), min(data[[x]], na.rm = TRUE), NA))
  fmax = sapply(features, function(x) ifelse(!is.factor(data[[x]]), max(data[[x]], na.rm = TRUE), NA))
  resample = "none"
  cutoff = 3L

  expect_that(generateFeatureGrid("w", data, resample, fmin["w"], fmax["w"], cutoff), is_a("numeric"))
  expect_that(generateFeatureGrid("x", data, resample, fmin["x"], fmax["x"], cutoff), is_a("factor"))
  expect_that(levels(generateFeatureGrid("x", data)), equals(letters[1:3]))
  expect_that(generateFeatureGrid("y", data, resample, fmin["y"], fmax["y"], cutoff), is_a("ordered"))
  expect_that(generateFeatureGrid("z", data, resample, fmin["z"], fmax["z"], cutoff), is_a("integer"))
})
