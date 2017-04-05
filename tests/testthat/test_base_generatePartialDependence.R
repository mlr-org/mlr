context("generatePartialDependence")

test_that("generateFunctionalANOVAData", {
  gridsize = 3L

  fr = train("regr.rpart", regr.task)
  dr1 = generateFunctionalANOVAData(fr, regr.task, c("lstat", "age"), 1L, mean, gridsize = gridsize)
  plotPartialDependence(dr1)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)

  dr2 = generateFunctionalANOVAData(fr, regr.task, c("lstat", "age"), 2L, mean, gridsize = gridsize)
  expect_that(dim(dr2$data), equals(c(gridsize^2, 4L)))
  expect_that(dr2$interaction, is_true())
  plotPartialDependence(dr2, "tile")
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)

  expect_error(generateFunctionalANOVAData(fr, regr.task, c("lstat", "age"), 3L, mean, gridsize = gridsize))

  dr1b = generateFunctionalANOVAData(fr, regr.task, c("lstat", "age"), 1L,
    function(x) quantile(x, c(.025, .5, .975)), gridsize = gridsize)
  expect_that(dim(dr1b$data), equals(c(gridsize * length(dr1b$features), 6L)))
  plotPartialDependence(dr1b)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)

  dr2b = generateFunctionalANOVAData(fr, regr.task, c("lstat", "age"), 2L,
    function(x) quantile(x, c(.025, .5, .975)), gridsize = gridsize)
  expect_that(dim(dr2b$data), equals(c(gridsize^length(dr2b$features), 6L)))
  expect_that(dr2b$interaction, is_true())
  plotPartialDependence(dr2b, "tile")
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)

  dr3 = generateFunctionalANOVAData(fr, regr.task, c("lstat", "age", "rm"), 2L)
  expect_error(plotPartialDependence(dr3, "tile"))
})

test_that("generatePartialDependenceData", {
  gridsize = 3L

  # test regression with interactions, centering, and mixed factor features
  fr = train("regr.rpart", regr.task)
  dr = generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE, fmin = list("lstat" = 1, "chas" = NA),
    fmax = list("lstat" = 40, "chas" = NA), gridsize = gridsize)
  nfeat = length(dr$features)
  nfacet = length(unique(regr.df[["chas"]]))
  n = getTaskSize(regr.task)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(gridsize * nfeat))
  expect_true(all(dr$data$medv >= min(regr.df$medv) | dr$data$medv <= max(regr.df$medv)))

  plotPartialDependence(dr, facet = "chas")
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfacet * gridsize))
  # plotPartialDependenceGGVIS(dr, interact = "chas")

  # check that if the input is a data.frame things work
  dr.df = generatePartialDependenceData(fr, input = regr.df, features = "lstat")

  # check that the interactions and centering work with ICE
  dr = generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE, individual = TRUE,
    fmin = list("lstat" = 1, "chas" = NA),
    fmax = list("lstat" = 40, "chas" = NA), gridsize = gridsize)
  expect_that(max(dr$data$lstat), equals(40.))
  expect_that(min(dr$data$lstat), equals(1.))
  expect_that(nrow(dr$data), equals(gridsize * nfeat * n))

  plotPartialDependence(dr, facet = "chas", data = regr.df, p = 1)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet))
  # black.circle.xpath counts points which are omitted when individual = TRUE
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfacet * gridsize * n + n))
  # plotPartialDependenceGGVIS(dr, interact = "chas")

  # check that multiple features w/o interaction work with a label outputting classifier with
  # an appropriate aggregation function
  fc = train("classif.rpart", multiclass.task)
  dc = generatePartialDependenceData(fc, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    fun = function(x) table(x) / length(x), gridsize = gridsize)
  nfeat = length(dc$features)
  n = getTaskSize(multiclass.task)
  plotPartialDependence(dc, data = multiclass.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  # minus one because the of the legend
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, red.circle.xpath, ns.svg)) - 1, equals(nfeat * gridsize))
  expect_that(length(XML::getNodeSet(doc, blue.circle.xpath, ns.svg)) - 1, equals(nfeat * gridsize))
  expect_that(length(XML::getNodeSet(doc, green.circle.xpath, ns.svg)) - 1, equals(nfeat * gridsize))
  # plotPartialDependenceGGVIS(dc)

  # test that an inappropriate function for a classification task throws an error
  # bounds cannot be used on classifiers
  fcp = train(makeLearner("classif.rpart", predict.type = "prob"), multiclass.task)
  expect_error(generatePartialDependence(fcp, input = multiclass.task, features = "Petal.Width",
    fun = function(x) quantile(x, c(.025, .5, .975))), gridsize = gridsize)

  # check that probability outputting classifiers work w/ interactions
  dcp = generatePartialDependenceData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    interaction = TRUE, gridsize = gridsize)
  nfacet = length(unique(dcp$data$Petal.Length))
  ntarget = length(dcp$target)
  plotPartialDependence(dcp, "tile")

  # check that probability outputting classifiers work with ICE
  dcp = generatePartialDependenceData(fcp, input = multiclass.task, features = c("Petal.Width", "Petal.Length"),
    interaction = TRUE, individual = TRUE, gridsize = gridsize)

  # check that survival tasks work with multiple features
  fs = train("surv.rpart", surv.task)
  ds = generatePartialDependenceData(fs, input = surv.task, features = c("x1", "x2"), gridsize = gridsize)
  nfeat = length(ds$features)
  n = getTaskSize(surv.task)
  plotPartialDependence(ds, data = surv.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(gridsize * nfeat))
  # plotPartialDependenceGGVIS(ds)

  # issue 1180 test
  pd = generatePartialDependenceData(fr, input = regr.task,
    features = c("lstat", "chas"), gridsize = gridsize)
  plotPartialDependence(pd)

  # check that bounds work for regression
  db = generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE,
    fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = gridsize)
  nfacet = length(unique(regr.df[["chas"]]))
  n = getTaskSize(regr.task)
  expect_that(colnames(db$data), equals(c("medv", "lstat", "chas", "lower", "upper")))
  plotPartialDependence(db, facet = "chas", data = regr.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfacet))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfacet * gridsize + n))
  # plotPartialDependenceGGVIS(db, interact = "chas")

  # check derivative and factor feature failure
  expect_error(generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    derivative = TRUE))

  # check interaction + derivative failure
  expect_error(generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "chas"),
    interaction = TRUE, derivative = TRUE))

  # check that bounds work w/o interaction
  db2 = generatePartialDependenceData(fr, input = regr.task, features = c("lstat", "crim"),
    fun = function(x) quantile(x, c(.25, .5, .75)), gridsize = gridsize)
  nfeat = length(db2$features)
  n = getTaskSize(regr.task)
  plotPartialDependence(db2, data = regr.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  expect_that(length(XML::getNodeSet(doc, black.circle.xpath, ns.svg)), equals(nfeat * gridsize + n * nfacet))
  # plotPartialDependenceGGVIS(db2)

  fcpb = train(makeLearner("classif.rpart", predict.type = "prob"), binaryclass.task)
  bc = generatePartialDependenceData(fcpb, input = binaryclass.task, features = c("V11", "V12"),
    individual = TRUE, gridsize = gridsize)
  # test for issue 1536
  bc.center1 = generatePartialDependenceData(fcpb, input = binaryclass.task, features = "V11",
    individual = TRUE, gridsize = gridsize,
    center = list("V11" = min(binaryclass.df$V11)))
  bc.center2 = generatePartialDependenceData(fcpb, input = binaryclass.task, features = c("V11", "V12"),
    individual = TRUE, gridsize = gridsize,
    center = list("V11" = min(binaryclass.df$V11), "V12" = min(binaryclass.df$V12)))
  nfeat = length(bc$features)
  n = getTaskSize(binaryclass.task)
  plotPartialDependence(bc, data = binaryclass.df)
  ggsave(path)
  doc = XML::xmlParse(path)
  expect_that(length(XML::getNodeSet(doc, grey.rect.xpath, ns.svg)), equals(nfeat))
  # again, omission of points for individual = TRUE
  expect_that(length(XML::getNodeSet(doc, red.line.xpath, ns.svg)), equals(nfeat * n))
  # plotPartialDependenceGGVIS(bc)

  # check that derivative estimation works for ICE and pd for classification and regression
  subset = 1:5
  fr = train(makeLearner("regr.ksvm"), regr.task)
  pfr = generatePartialDependenceData(fr, input = regr.df[subset, ], features = c("lstat", "crim"),
    derivative = TRUE, individual = TRUE, gridsize = gridsize)
  fc = train(makeLearner("classif.ksvm", predict.type = "prob"), multiclass.task)
  pfc = generatePartialDependenceData(fc, input = multiclass.df[subset, ],
    features = c("Petal.Width", "Petal.Length"),
    derivative = TRUE, gridsize = gridsize)
  fs = train(makeLearner("surv.coxph"), surv.task)
  pfs = generatePartialDependenceData(fs, input = surv.df[subset, ],
    features = c("x1", "x2"),
    derivative = TRUE, gridsize = gridsize)

  # check that se estimation works
  fse = train(makeLearner("regr.lm", predict.type = "se"), regr.task)
  pfse = generatePartialDependenceData(fse, input = regr.task, features = c("lstat", "crim"),
    bounds = c(-2, 2), gridsize = gridsize)

  # check that tile + contour plots work for two and three features with regression and survival
  expect_error(plotPartialDependence(ds, geom = "tile")) # interaction == FALSE
  tfr = generatePartialDependenceData(fr, regr.df, features = c("lstat", "crim", "chas"),
    interaction = TRUE, gridsize = gridsize)
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

  # with the joint distribution as the weight function generatePartialDependenceData
  # should return NA for regions with zero probability
  x = runif(50L)
  y = 2 * x
  idx = which(x > .5)
  x[idx] = NA
  test.task = makeRegrTask(data = data.frame(x = x[-idx], y = y[-idx]), target = "y")
  fit = train("regr.rpart", test.task)

  fun = function(x, newdata) {
    w = ifelse(newdata$x > .5, 0, 1)
    if (sum(w) == 0) {
      NA
    } else {
      weighted.mean(x, w)
    }
  }

  pd = generatePartialDependenceData(fit, test.task, fun = fun,
    fmin = list("x" = 0), fmax = list("x" = 1), gridsize = gridsize)
  expect_that(all(is.na(pd$data[pd$data$x > .5, "y"])), is_true())

  # issue 55 in the tutorial
  pd = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    center = list("Petal.Width" = min(multiclass.df$Petal.Width)), gridsize = gridsize)

  # subsequent bug found in pr #1206
  pd = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    individual = TRUE,
    center = list("Petal.Width" = min(multiclass.df$Petal.Width)), gridsize = gridsize)

  # issue 63 in the tutorial
  pd = generatePartialDependenceData(fcp, multiclass.task, "Petal.Width",
    individual = TRUE, derivative = TRUE, gridsize = gridsize)
})

test_that("generateFeatureGrid", {
  data = data.frame(
    w = seq(0, 1, length.out = 5),
    x = factor(letters[1:5]),
    y = ordered(1:5),
    z = 1:5
  )
  gridsize = 3
  features = colnames(data)
  fmin = sapply(features, function(x)
    ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      min(data[[x]], na.rm = TRUE), NA), simplify = FALSE)
  fmax = sapply(features, function(x)
    ifelse(is.ordered(data[[x]]) | is.numeric(data[[x]]),
      max(data[[x]], na.rm = TRUE), NA), simplify = FALSE)

  out = generateFeatureGrid(features, data, "none", gridsize = gridsize, fmin, fmax)
  expect_true(all(sapply(out, length) == gridsize))
  expect_that(out$w, is_a("numeric"))
  expect_that(range(out$w), equals(range(data$w)))
  expect_that(length(out$w), equals(gridsize))
  expect_that(out$x, is_a("factor"))
  expect_that(length(out$x), equals(gridsize))
  expect_that(levels(out$x), equals(levels(data$x)))
  expect_that(out$y, is_a("ordered"))
  expect_that(levels(out$y), equals(levels(data$y)))
  expect_that(range(out$y), equals(range(data$y)))
  expect_that(out$z, is_a("integer"))
  expect_that(range(out$z), equals(range(data$z)))

  out.sub = generateFeatureGrid(features, data, "subsample",
    gridsize = gridsize, fmin, fmax)
  expect_true(all(sapply(out.sub, length) == gridsize))
  expect_that(out.sub$w, is_a("numeric"))
  expect_that(length(out.sub$w), equals(gridsize))
  expect_that(out.sub$x, is_a("factor"))
  expect_that(length(out.sub$x), equals(gridsize))
  expect_that(levels(out.sub$x), equals(levels(data$x)))
  expect_that(out.sub$y, is_a("ordered"))
  expect_that(levels(out.sub$y), equals(levels(data$y)))
  expect_that(out.sub$z, is_a("integer"))
})
