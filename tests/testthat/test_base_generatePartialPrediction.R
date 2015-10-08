context("generatePartialPrediction")

test_that("generatePartialPredictionData", {
  gridsize = 3L

  fr = train("regr.rpart", regr.task)
  dr = generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "chas"),
                                     interaction = TRUE, fmin = list("lstat" = 1, "chas" = NA),
                                     fmax = list("lstat" = 40, "chas" = NA), gridsize = gridsize)
  nfeat = length(dr$features)
  nfacet = length(unique(getTaskData(regr.task)[["chas"]]))
  n = getTaskSize(regr.task)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(gridsize * nfeat))

  plotPartialPrediction(dr, facet = "chas")
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(nfacet * gridsize))
  ## plotPartialPredictionGGVIS(dr, interact = "chas")

  dr.df = generatePartialPredictionData(fr, input = getTaskData(regr.task), features = "lstat")

  dr.1 = generatePartialPredictionData(fr, input = regr.task, features = "lstat",
                                       fmin = list("lstat" = 1),
                                       fmax = list("lstat" = 40), gridsize = gridsize)

  dr = generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "chas"),
                                     interaction = TRUE, individual = TRUE,
                                     fmin = list("lstat" = 1, "chas" = NA),
                                     fmax = list("lstat" = 40, "chas" = NA), gridsize = gridsize)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(gridsize * nfeat * n))

  plotPartialPrediction(dr, facet = "chas")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfacet))
  ## black.xpath counts points which are omitted when individual = TRUE
  ## expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(nfacet * gridsize * n))
  ## plotPartialPredictionGGVIS(dr, interact = "chas")

  fc = train("classif.rpart", multiclass.task)
  dc = generatePartialPredictionData(fc, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
                                     fun = function(x) table(x) / length(x), gridsize = gridsize)
  nfeat = length(dc$features)
  n = getTaskSize(multiclass.task)
  plotPartialPrediction(dc)
  ggsave(path)
  doc = XML::xmlParse(path)
  ## minus one because the of the legend
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(nfeat * gridsize))
  expect_that(length(XML::getNodeSet(doc, blue.xpath, "svg")) - 1, equals(nfeat * gridsize))
  expect_that(length(XML::getNodeSet(doc, green.xpath, "svg")) - 1, equals(nfeat * gridsize))
  ## plotPartialPredictionGGVIS(dc)

  fcp = train(makeLearner("classif.rpart", predict.type = "prob"), multiclass.task)
  expect_error(generatePartialPrediction(fcp, input = multiclass.task, features = "Petal.Width",
                                         fun = function(x) quantile(x, c(.025, .5, .975))), gridsize = gridsize)
  dcp = generatePartialPredictionData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
                                      interaction = TRUE, gridsize = gridsize)
  nfacet = length(unique(dcp$data$Petal.Length))
  ntarget = length(dcp$target)
  plotPartialPrediction(dcp, facet = "Petal.Length")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(ntarget * nfacet))
  expect_that(length(XML::getNodeSet(doc, blue.xpath, "svg")) - 1, equals(ntarget * nfacet))
  expect_that(length(XML::getNodeSet(doc, green.xpath, "svg")) - 1, equals(ntarget * nfacet))
  ## plotPartialPredictionGGVIS(dcp, interact = "Petal.Length")

  dcp = generatePartialPredictionData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
                                      interaction = TRUE, individual = TRUE, gridsize = gridsize)
  plotPartialPrediction(dcp, facet = "Petal.Length")
  ## plotPartialPredictionGGVIS(dcp, interact = "Petal.Length")

  fs = train("surv.rpart", surv.task)
  ds = generatePartialPredictionData(fs, input = surv.task, features = c("Petal.Width", "Petal.Length"),
                                     gridsize = gridsize)
  nfeat = length(ds$features)
  n = getTaskSize(surv.task)
  plotPartialPrediction(ds)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(gridsize * nfeat))
  ## plotPartialPredictionGGVIS(ds)

  db = generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "chas"),
                                     interaction = TRUE,
                                     fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = gridsize)
  nfacet = length(unique(getTaskData(regr.task)[["chas"]]))
  expect_error(generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "chas"),
                                             derivative = TRUE))
  expect_error(generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "chas"),
                                             interaction = TRUE, derivative = TRUE))
  n = getTaskSize(regr.task)
  expect_that(colnames(db$data), equals(c("medv", "lstat", "chas", "lower", "upper")))
  plotPartialPrediction(db, facet = "chas")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(nfacet * gridsize))
  ## plotPartialPredictionGGVIS(db, interact = "chas")

  db2 = generatePartialPredictionData(fr, input = regr.task, features = c("lstat", "crim"),
                                      fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = gridsize)
  nfeat = length(db2$features)
  n = getTaskSize(regr.task)
  plotPartialPrediction(db2)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.xpath, "svg")), equals(nfeat * gridsize))
  ## plotPartialPredictionGGVIS(db2)

  fcpb = train(makeLearner("classif.rpart", predict.type = "prob"), binaryclass.task)
  bc = generatePartialPredictionData(fcpb, input = binaryclass.task, features = c("V11", "V12"),
                                     individual = TRUE, gridsize = gridsize)
  nfeat = length(bc$features)
  n = getTaskSize(binaryclass.task)
  plotPartialPrediction(bc)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.xpath, "svg")), equals(nfeat))
  ## again, omission of points for individual = TRUE
  ## expect_that(length(XML::getNodeSet(doc, red.xpath, "svg")) - 1, equals(nfeat * gridsize * n))
  ## plotPartialPredictionGGVIS(bc)

  subset = 1:5
  fr = train(makeLearner("regr.ksvm"), regr.task)
  pfr = generatePartialPredictionData(fr, input = regr.df[subset, ], features = c("lstat", "crim"),
                                      derivative = TRUE, individual = TRUE, gridsize = gridsize)
  fc = train(makeLearner("classif.ksvm", predict.type = "prob"), multiclass.task)
  pfc = generatePartialPredictionData(fc, input = multiclass.df[subset, ],
                                      features = c("Petal.Width", "Petal.Length"),
                                      derivative = TRUE, gridsize = gridsize)
  fs = train(makeLearner("surv.coxph"), surv.task)
  pfs = generatePartialPredictionData(fs, input = surv.df[subset, ],
                                      features = c("Petal.Width", "Petal.Length"),
                                      derivative = TRUE, gridsize = gridsize)

  fse = train(makeLearner("regr.lm", predict.type = "se"), regr.task)
  pfse = generatePartialPredictionData(fse, input = regr.task, features = c("lstat", "crim"),
                                       bounds = c(-2, 2), gridsize = gridsize)
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
  expect_that(levels(generateFeatureGrid("x", data, resample, NA, NA, cutoff)), equals(letters[1:3]))
  expect_that(generateFeatureGrid("y", data, resample, fmin["y"], fmax["y"], cutoff), is_a("ordered"))
  expect_that(generateFeatureGrid("z", data, resample, fmin["z"], fmax["z"], cutoff), is_a("integer"))
})
