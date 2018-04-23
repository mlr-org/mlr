context("extactFDAFeatures")

test_that("extractFDAFeatures", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(), "NIR" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129L)
  expect_subset(colnames(df), c(paste0("NIR.phase", seq_len(231)),
    paste0("UVVIS.multires.", seq_len(9)), "heatan", "h20"))
})

test_that("extractFeatures multiple times", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(),
    "UVVIS" = extractFDAFourier(),
    "NIR" = extractFDAMultiResFeatures())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_class(df, "data.frame")
  expect_true(nrow(df) == 129L)
  expect_true(ncol(df) == 154L)
  expect_subset(colnames(df), c("heatan", "h20", paste0("UVVIS.phase", seq_len(134)),
    paste0("NIR.multires.", seq_len(9)), paste0("UVVIS.multires.", seq_len(9))))

  methods = list("all" = extractFDAMultiResFeatures(), "all" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_true(nrow(df) == 129L)
  expect_true(ncol(df) == 385L)
  expect_subset(colnames(df),
    c("heatan", "h20",
      paste0("UVVIS.multires.", seq_len(9)), paste0("NIR.multires.", seq_len(9)),
      paste0("UVVIS.phase", seq_len(134)), paste0("NIR.phase", seq_len(231))))
})


test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1:30)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR.phase]", all = TRUE)
})


test_that("Wrong methods yield errors", {
  t = subsetTask(fuelsubset.task, subset = 1:2)

  wrng1 = function() {
    lrn = function(data, target, col, vals = NULL) {1}
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, par.set = makeParamSet())
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng1())),
    "feat.method needs to return")


  wrng2 = function() {
    lrn = function(data) {data[, 1]}
    makeExtractFDAFeatMethod(learn = lrn, reextract = lrn, par.set = makeParamSet())
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng2())),
    "Must have formal arguments")

  wrng3 = function() {
    lrn = function(data, target, col, vals = NULL) {data.frame(1)}
    makeExtractFDAFeatMethod(z = lrn, rz = lrn)
  }
  expect_error(extractFDAFeatures(t, feat.methods = list("NIR" = wrng3())),
    "unused arguments")
})

test_that("extractFDAFeatures colnames work", {
  methods = list("NIR" = extractFDAFourier())
  t = subsetTask(fuelsubset.task, subset = 1)
  t2 = extractFDAFeatures(t, feat.methods = methods)
  cn = getTaskFeatureNames(t2$task)
  expect_match(setdiff(cn, "h2o"), regexp = "[NIR.phase]", all = TRUE)
})

test_that("extractFDAFeaturesDesc", {
  methods = list("UVVIS" = extractFDAMultiResFeatures(), "NIR" = extractFDAFourier())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check desc
  expect_is(t$desc, "extractFDAFeatDesc")
  expect_subset(t$desc$coln, c(getTaskFeatureNames(fuelsubset.task),
    getTaskTargetNames(fuelsubset.task)))
  expect_subset(t$desc$target, getTaskTargetNames(fuelsubset.task))
  expect_subset(unique(t$desc$colclasses), choices = c("numeric", "matrix"))
  expect_list(t$desc$extractFDAFeat)
  expect_list(t$desc$extractFDAFeat$UVVIS$args)
  expect_function(t$desc$extractFDAFeat$UVVIS$reextract)
  expect_list(t$desc$extractFDAFeat$NIR$args)
  expect_function(t$desc$extractFDAFeat$NIR$reextract)
})

test_that("extractFDAFeatures task equal data.frame", {
  # check data.frame output equal to task's data output
  gp.subset = subsetTask(gunpoint.task, features = 1L)
  fm = list("fd" = extractFDAFourier(trafo.coeff = "amplitude"))
  t2 = extractFDAFeatures(gp.subset, feat.methods = fm)
  gp.desc = getTaskDesc(gp.subset)
  t3 = extractFDAFeatures(getTaskData(gp.subset, functionals.as = "matrix"), target = "X1", feat.methods = fm)
  expect_identical(getTaskData(t2$task), t3$data)
  expect_equal(t2$desc, t3$desc)
  expect_equal(t2$desc$extractFDAFeat$fd$arg$trafo.coeff, "amplitude")

  expect_error(extractFDAFeatures(gp.subset, feat.methods = list("fd" = extractFDAFourier(),
    "fd2" = extractFDAMultiResFeatures())), regexp = "Must be a subset of")
})

test_that("reextractFDAFeatures", {
  gp.subset = subsetTask(gunpoint.task, features = 1L)
  fm = list("fd" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})

test_that("extract reextract feat.methods all", {
  fm2 = list("all" = extractFDAFourier(trafo.coeff = "amplitude"))
  t3 = extractFDAFeatures(fuelsubset.task, feat.methods = fm2)
  t4 = reextractFDAFeatures(fuelsubset.task, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})



test_that("Wavelet method are equal to package", {
  requirePackagesOrSkip("wavelets", default.method = "load")
  lrn = extractFDAWavelets()$learn
  gp = getTaskData(gunpoint.task, subset = 1:10, target.extra = TRUE, functionals.as = "matrix")
  # Method
  set.seed(getOption("mlr.debug.seed"))
  wavelets.gp = lrn(data = gp$data, target = "X1", col = "fd", filter = "haar", boundary = "reflection")

  # Reference
  df = BBmisc::convertRowsToList(gp$data[, "fd", drop = FALSE])
  set.seed(getOption("mlr.debug.seed"))
  wtdata = t(BBmisc::dapply(df, fun = function(x) {
    wt = wavelets::dwt(as.numeric(x), filter = "haar", boundary = "reflection")
    unlist(c(wt@W, wt@V[[wt@level]]))
  }))
  df = as.data.frame(wtdata)
  colnames(df) = stringi::stri_paste("wav", "haar", seq_len(ncol(wtdata)), sep = ".")

  expect_equal(nrow(wavelets.gp), nrow(gp$data))
  expect_equal(wavelets.gp, df)
  # Too many vanishing moments (support width) expected
  expect_error(extractWaveletFeatures(data = gp, filter = "d10"))
})

test_that("extract and reextract Wavelets", {
  requirePackagesOrSkip("wavelets", default.method = "load")
  gp.subset = subsetTask(gunpoint.task, features = 1L)
  fm = list("fd" = extractFDAWavelets(filter = "haar", boundary = "reflection"))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
})



test_that("getUniFDAMultiResFeatures works on data.frame", {
  gp = getTaskData(fda.binary.gp.task.small, functionals.as = "matrix")
  ngp1 = extractFDAMultiResFeatures()$learn(data = gp, col = "fd", res.level = 3,
    shift = 0.5, seg.lens = NULL)
  expect_true(nrow(ngp1) == nrow(gp))
  expect_true(ncol(ngp1) == 9L)
  ngp2 = extractFDAMultiResFeatures()$learn(data = gp, col = "fd", seg.lens = c(25, 25),
    res.level = 3, shift = 0.5)
  expect_true(nrow(ngp2) == nrow(gp))
  expect_true(ncol(ngp2) == 16L)
})

test_that("get...FDAMultiResFeatures works on data.frame", {
  df = getTaskData(fuelsubset.task, functionals.as = "matrix")

  lrn = extractFDAMultiResFeatures()$learn
  dfn = lrn(df, col = "UVVIS", res.level = 3L, shift = 0.5, seg.lens = NULL)
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 9L)

  dfn2 = lrn(df, col = "NIR", res.level = 3L, shift = 0.5, seg.lens = NULL)
  expect_true(nrow(df) == nrow(dfn2))
  expect_true(ncol(dfn2) == 9L)

  expect_true(!all(dfn == dfn2))

  dfn = lrn(df, col = "NIR", res.level = 3L, shift = 0.5, seg.lens = c(100L, 131L))
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 19L)

  dfn = lrn(df, col = "NIR", res.level = 3L, shift = 0.5, seg.lens = 231L)
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 9L)

  dfn = lrn(df, col = "NIR", res.level = 1L, shift = 0.5, seg.lens = 231L)
  expect_true(nrow(df) == nrow(dfn))
  expect_true(ncol(dfn) == 1L)
})

test_that("extract and reextract MultiRes", {
  gp.subset = subsetTask(gunpoint.task, subset = 1:20, features = 1L)
  fm = list("fd" = extractFDAMultiResFeatures(3L, 0.4))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
  expect_equal(t3$task$task.desc$n.feat["numerics"], c(numerics = 12L))
})



test_that("extractFPCAFeatures is equivalent to package", {
  requirePackagesOrSkip(c("mboost", "refund"), default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  lrn = extractFDAFPCA()$learn
  gp = getTaskData(gunpoint.task, subset = 1:10, target.extra = TRUE, functionals.as = "matrix")
  fpca.df = lrn(data = gp$data, target = "X1", col = "fd", pve = 0.99, npc = NULL)
  expect_true((nrow(gp$data) == nrow(fpca.df)))
  expect_true((ncol(fpca.df) == 5L))
  expect_match(names(fpca.df), regexp = "[FPCA]")

  # Is it equivalent to the mlr version?
  set.seed(getOption("mlr.debug.seed"))
  fpca.df2 = data.frame(refund::fpca.sc(Y = as.matrix(gp$data$fd))$scores)
  expect_true((nrow(gp$data) == nrow(fpca.df2)))
  expect_true((ncol(fpca.df2) == 5L))
  expect_equivalent(fpca.df, fpca.df2)

  set.seed(getOption("mlr.debug.seed"))
  gp = getTaskData(gunpoint.task, subset = 1:20, target.extra = TRUE, functionals.as = "matrix")
  fpca.df = lrn(data = gp$data, target = "X1", col = "fd", npc = 12L)
  expect_true((nrow(gp$data) == nrow(fpca.df)))
  expect_true((ncol(fpca.df) == 12L))
  expect_match(names(fpca.df), regexp = "[FPCA]")
})

test_that("extract and reextract FPCA", {
  requirePackagesOrSkip(c("mboost", "refund"), default.method = "load")
  gp.subset = subsetTask(gunpoint.task, subset = 1:20, features = 1L)
  fm = list("fd" = extractFDAFPCA(pve = .9, npc = 10))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
  expect_equal(t3$task$task.desc$n.feat["numerics"], c(numerics = 10L))
})



test_that("Fourier equal to package", {
  gp1 = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8)
  lrn = extractFDAFourier()$learn
  fourier.gp = lrn(data = gp1, trafo.coeff = "phase")
  expect_equal(nrow(fourier.gp), nrow(gp1))
  # Phase (arctan(...) in range(-pi/2, pi/2) )
  expect_true(all(fourier.gp < pi / 2 & fourier.gp > - pi / 2))

  fourier.a.gp = lrn(data = gp1, trafo.coeff = "amplitude")
  expect_equal(nrow(fourier.a.gp), nrow(gp1))
  # Amplitude sqrt(Re^2 + Im^2) >= 0
  expect_true(all(fourier.a.gp >= 0))

  # Calculate fourier coefficients (row wise) which are complex numbers
  fft.trafo = t(apply(gp1, 1, fft))
  # Extract amplitude or phase of fourier coefficients which are real numbers
  fft.pa = switch("amplitude",
    amplitude = sqrt(apply(fft.trafo, 2, function(x) Re(x)^2 + Im(x)^2)),
    phase = apply(fft.trafo, 2, function(x) atan(Im(x) / Re(x)))
  )

  # If there is only one row in data, fft returns an array
  if (!inherits(fft.pa, "matrix")) {
    fft.pa = as.data.frame(matrix(fft.pa, nrow = 1))
  }
  # Add more legible column names to the output
  df = as.data.frame(fft.pa)
  colnames(df) = stringi::stri_paste("amplitude", seq_len(ncol(fft.pa)))

  expect_equal(df, fourier.a.gp)

  # Can not have factors
  gp2 = data.frame(v1  =  t(1:4), X1 = as.factor(1))
  expect_error(extractFourierFeatures(data = gp2, trafo.coeff = "amplitude"))
})


test_that("tsfeatures works", {

  requirePackagesOrSkip("tsfeatures")
  gp1 = getTaskData(fuelsubset.task, functionals.as = "matrix")[1:30, ]
  lrn = extractFDATsfeatures()$learn
  gpfeats = lrn(data = gp1, col = "UVVIS")
  expect_equal(nrow(gpfeats), nrow(gp1))

  extr = extractFDAFeatures(subsetTask(fuelsubset.task, subset = 1:30, features = 2), feat.methods = list("UVVIS" = extractFDATsfeatures()))
  # FIXME: Decide on extraction subset before testing versus method.
  reextr = reextractFDAFeatures(subsetTask(fuelsubset.task, subset = 31:35), extr$desc)
  # FIXME: Tests
})

test_that("dtw extract works", {
  requirePackagesOrSkip("rucrdtw")
  task = subsetTask(fuelsubset.task, features = "UVVIS")
  daf = getTaskData(task, functionals.as = "matrix")
  daf = daf$UVVIS
  fmethods = list("UVVIS" = extractFDADTWKernel())
  res = extractFDAFeatures(fuelsubset.task, feat.methods = fmethods)
  # check output data
  df = getTaskData(res$task, functionals.as = "matrix")
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129)
  expect_equal(ncol(df), 9)
})

test_that("extraction returns correct cols", {
  requirePackagesOrSkip("tsfeatures")
  extr = extractFDAFeatures(subsetTask(fuelsubset.task, subset = 1:2), feat.methods = list("UVVIS" = extractFDATsfeatures()))
  reextr = reextractFDAFeatures(subsetTask(fuelsubset.task, subset = 3:10), extr$desc)
  expect_equal(extr$task$task.desc$n.feat, reextr$task.desc$n.feat)
  expect_equal(colnames(getTaskData(extr$task, functionals.as = "matrix")),
    colnames(getTaskData(reextr, functionals.as = "matrix")))
})


test_that("extract and reextract have correct args", {
  lrn = makeExtractFDAFeatsWrapper("regr.rpart", feat.methods = list("all" = extractFDAFourier()))
  mod = train(setHyperPars(lrn, trafo.coeff = "amplitude"), subsetTask(fuelsubset.task, subset = 1:20))
  prd = predict(mod, subsetTask(fuelsubset.task, subset = 21:40))
  expect_equal(mod$learner.model$control$extractFDAFeat$UVVIS$args$trafo.coeff, "amplitude")
  expect_equal(mod$learner.model$control$extractFDAFeat$NIR$args$trafo.coeff, "amplitude")
})
