context("generatePartialDependence")

test_that("generatePartialDependenceData", {
  m = c(4, 10)

  # test regression with interactions, centering, and mixed factor features
  fr = train("regr.rpart", regr.task)
  dr = generatePartialDependenceData(fr, input = regr.task,
    features = c("lstat", "chas"), interaction = TRUE, n = m)
  nfeat = length(dr$features)
  nfacet = length(unique(regr.df[["chas"]]))
  n = getTaskSize(regr.task)
  expect_that(nrow(dr$data), equals(m[1] * nfeat))
  expect_true(all(dr$data$medv >= min(regr.df$medv) | dr$data$medv <= max(regr.df$medv)))

  plotPartialDependence(dr, facet = "chas")
  dir = tempdir()
  path = file.path(dir, "test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfacet * m[1]))

  # check that if the input is a data.frame things work
  dr.df = generatePartialDependenceData(fr, input = regr.df, features = "lstat")

  # check that the interactions and centering work with ICE
  dr = generatePartialDependenceData(fr, input = regr.task,
    features = c("lstat", "chas"), interaction = TRUE, individual = TRUE, n = m)
  expect_that(nrow(dr$data), equals(m[1] * nfeat * m[2]))

  plotPartialDependence(dr, facet = "chas", data = regr.df, p = 1)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet))
  # black.circle.xpath counts points which are omitted when individual = TRUE
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(n + prod(m) * nfacet))

  # check that multiple features w/o interaction work with a label outputting classifier with
  # an appropriate aggregation function
  fc = train("classif.rpart", multiclass.task)
  dc = generatePartialDependenceData(fc, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    fun = function(x) table(x) / length(x), n = m)
  nfeat = length(dc$features)
  n = getTaskSize(multiclass.task)
  plotPartialDependence(dc, data = multiclass.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  # minus one because the of the legend
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)) - 1, equals(nfeat * m[1]))
  expect_that(length(XML::getNodeSet(doc, blue.circle.xpath, ns.svg)) - 1, equals(nfeat * m[1]))
  expect_that(length(XML::getNodeSet(doc, green.circle.xpath, ns.svg)) - 1, equals(nfeat * m[1]))

  fcp = train(makeLearner("classif.svm", predict.type = "prob"), multiclass.task)

  # test that with classif but predict.type = "response" we can use fun which
  # return a vector
  dcp = generatePartialDependenceData(fcp, input = multiclass.task,
    features = "Petal.Width",
    fun = function(x) quantile(x, c(.025, .5, .975)), n = m)
  plotPartialDependence(dcp)

  # check that probability outputting classifiers work w/ interactions
  dcp = generatePartialDependenceData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    interaction = TRUE, n = m)
  nfacet = length(unique(dcp$data$Petal.Length))
  ntarget = length(dcp$target)
  plotPartialDependence(dcp, "tile")

  # check that probability outputting classifiers work with ICE
  dcp = generatePartialDependenceData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    interaction = TRUE, individual = TRUE, n = m)

  # check that survival tasks work with multiple features
  fs = train("surv.rpart", surv.task)
  ds = generatePartialDependenceData(fs, input = surv.task, features = c("x1", "x2"), n = m)
  nfeat = length(ds$features)
  n = getTaskSize(surv.task)
  plotPartialDependence(ds, data = surv.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(m[1] * nfeat))

  # issue 1180 test
  pd = generatePartialDependenceData(fr, input = regr.task,
    features = c("lstat", "chas"), n = m)
  plotPartialDependence(pd)

  # check that bounds work for regression
  db = generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE,
    fun = function(x) quantile(x, c(.25, .5, .75)), n = m)
  nfacet = length(unique(regr.df[["chas"]]))
  n = getTaskSize(regr.task)
  expect_that(colnames(db$data), equals(c("medv", "Function", "lstat", "chas")))
  plotPartialDependence(db, facet = "chas", data = regr.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet * 3))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfacet * 3 * m[1] + n * 3))

  # check derivative and factor feature failure
  expect_error(generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    derivative = TRUE))

  # check interaction + derivative failure
  expect_error(generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE, derivative = TRUE))

  fcpb = train(makeLearner("classif.rpart", predict.type = "prob"), binaryclass.task)
  bc = generatePartialDependenceData(fcpb, input = binaryclass.task, features = c("V11", "V12"),
    individual = TRUE, n = m)
  ## tests for binary classification plotting, discovered whilst trying to merge
  ## pr #142
  plotPartialDependence(bc)
  bc = generatePartialDependenceData(fcpb, input = binaryclass.task, features = c("V11", "V12"), n = m)
  plotPartialDependence(bc)

  # check that derivative estimation works for ICE and pd for classification and regression
  fr = train(makeLearner("regr.ksvm"), regr.task)
  pfr = generatePartialDependenceData(fr, input = regr.df, features = c("lstat", "crim"),
    derivative = TRUE, individual = FALSE, n = m)
  pfri = generatePartialDependenceData(fr, input = regr.df,
    features = c("lstat", "crim"),
    derivative = TRUE, individual = TRUE, n = m)

  fc = train(makeLearner("classif.ksvm", predict.type = "prob"), multiclass.task)
  pfc = generatePartialDependenceData(fc, input = multiclass.df,
    features = c("Petal.Width", "Petal.Length"),
    derivative = TRUE, n = m)
  fs = train(makeLearner("surv.coxph"), surv.task)
  pfs = generatePartialDependenceData(fs, input = surv.df,
    features = c("x1", "x2"),
    derivative = TRUE, n = m)

  # check that se estimation works
  fse = train(makeLearner("regr.lm", predict.type = "se"), regr.task)
  pfse = generatePartialDependenceData(fse, input = regr.task, features = c("lstat", "crim"),
    bounds = c(-2, 2), n = m)
  plotPartialDependence(pfse)

  # check that tile + contour plots work for two and three features with regression and survival
  expect_error(plotPartialDependence(ds, geom = "tile")) # interaction == FALSE
  tfr = generatePartialDependenceData(fr, regr.df, features = c("lstat", "crim", "chas"),
    interaction = TRUE, n = m)
  plotPartialDependence(tfr, geom = "tile", facet = "chas", data = regr.df)
  tfs = generatePartialDependenceData(fs, surv.df, c("x1", "x2"), interaction = TRUE)
  plotPartialDependence(tfs, geom = "tile", data = surv.df)

  # facetting works with plotPartialDependence:
  q = plotPartialDependence(dr, facet = "chas", data = regr.df,
    facet.wrap.nrow = 2L)
  testFacetting(q, 2L)
  q = plotPartialDependence(dr, facet = "chas", facet.wrap.ncol = 2L,
    data = regr.df)
  testFacetting(q, ncol = 2L)

  # bug found in pr #1206
  pd = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    individual = TRUE, n = m)

  # issue 63 in the tutorial
  pd = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    individual = TRUE, derivative = TRUE, n = m)

  # test that would have caught a bug that occurs when the jacobian is estimated
  pd.der.classif = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    derivative = TRUE, n = m)
})
