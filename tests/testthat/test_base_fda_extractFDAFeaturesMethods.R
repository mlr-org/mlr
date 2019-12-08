context("extactFDAFeaturesMethods")

test_that("Wavelet method are equal to package", {
  requirePackagesOrSkip("wavelets", default.method = "load")
  gp = getTaskData(gunpoint.task, subset = seq_len(10), target.extra = TRUE, functionals.as = "matrix")

  # Extractor
  extr = extractFDAWavelets()
  wav.vals = extr$learn(data = gp$data, target = "X1", col = "fd", filter = "la8", boundary = "reflection")
  wavelets.gp = extr$reextract(data = gp$data, target = "X1", col = "fd", vals = wav.vals, args = NULL)

  # Reference
  df = BBmisc::convertRowsToList(gp$data[, "fd", drop = FALSE])
  wtdata = t(BBmisc::dapply(df, fun = function(x) {
    wt = wavelets::dwt(as.numeric(x), filter = "la8", boundary = "reflection")
    unlist(c(wt@W, wt@V[[wt@level]]))
  }))
  df = as.data.frame(wtdata)
  colnames(df) = stringi::stri_paste("wav", "la8", seq_len(ncol(wtdata)), sep = ".")

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

test_that("getFDAMultiResFeatures works on data.frame", {
  gp = getTaskData(fda.binary.gp.task.small, functionals.as = "matrix")

  ngp1 = extractFDAMultiResFeatures()$learn(data = gp, col = "fd", res.level = 3,
    shift = 0.5, seg.lens = NULL)
  ngp1 = extractFDAMultiResFeatures()$reextract(data = gp, col = "fd", vals = ngp1)
  expect_true(nrow(ngp1) == nrow(gp))
  expect_true(ncol(ngp1) == 9L)

  ngp2 = extractFDAMultiResFeatures()$learn(data = gp, col = "fd", seg.lens = c(15, 15),
    res.level = 3, shift = 0.5)
  ngp2 = extractFDAMultiResFeatures()$reextract(data = gp, col = "fd", vals = ngp2)
  expect_true(nrow(ngp2) == nrow(gp))
  expect_true(ncol(ngp2) == 18L)

  df = getTaskData(fuelsubset.task, functionals.as = "matrix")

  # Learn args equal to method args
  vals1 = extractFDAMultiResFeatures()$learn(df, col = "UVVIS", res.level = 2L, shift = 0.25)
  dfn1 = extractFDAMultiResFeatures()$reextract(df, col = "UVVIS", vals = vals1)
  expect_true(nrow(df) == nrow(dfn1))
  expect_true(ncol(dfn1) == 5L)

  fm = list("UVVIS" = extractFDAMultiResFeatures(res.level = 2L, shift = 0.25))
  dfn2 = extractFDAFeatures(df, feat.methods = fm)
  expect_true(nrow(df) == nrow(dfn2$data))
  expect_true(ncol(dfn2$data) == 5L + 3L)
  expect_equal(dfn1, setNames(dfn2$data[, seq_len(5)], colnames(dfn1)))


  extr = extractFDAMultiResFeatures()
  vals3 = extr$learn(df, col = "NIR", res.level = 3L, shift = 0.5, seg.lens = c(100L, 131L))
  dfn3 = extr$reextract(df, col = "NIR", vals = vals3)
  expect_true(nrow(df) == nrow(dfn3))
  expect_true(ncol(dfn3) == 19L)


  extr = extractFDAMultiResFeatures()
  vals4 = extr$learn(df, col = "NIR", res.level = 1L, shift = 0.2, seg.lens = 231L)
  dfn4 = extr$reextract(df, col = "NIR", vals = vals4)
  expect_true(nrow(df) == nrow(dfn4))
  expect_true(ncol(dfn4) == 1L)
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

test_that("extractFPCAFeatures is equivalent to prcomp", {
  gp = getTaskData(gunpoint.task, subset = 1:10, target.extra = TRUE, functionals.as = "matrix")

  extr = extractFDAFPCA()
  fpca.vals = extr$learn(data = gp$data, target = "X1", col = "fd", rank. = 5L)
  fpca.df = extr$reextract(data = gp$data, target = "X1", col = "fd", vals = fpca.vals)
  expect_true((nrow(gp$data) == nrow(fpca.df)))
  expect_true((ncol(fpca.df) == 5L))
  expect_match(names(fpca.df), regexp = "[FPCA]")

  # Is it equivalent to the mlr version?
  gp.mat = gp$data$fd
  fpca.df2 = predict(prcomp(gp.mat, rank. = 5L), gp.mat)
  expect_true((nrow(gp.mat) == nrow(fpca.df2)))
  expect_true((ncol(fpca.df2) == 5L))
  expect_equivalent(fpca.df, data.frame(fpca.df2))
})

test_that("extract and reextract FPCA", {
  gp.subset = subsetTask(gunpoint.task, subset = 1:20, features = 1L)
  fm = list("fd" = extractFDAFPCA(rank. = 5))
  t3 = extractFDAFeatures(gp.subset, feat.methods = fm)
  t4 = reextractFDAFeatures(gp.subset, t3$desc)
  expect_equal(getTaskFeatureNames(t3$task), getTaskFeatureNames(t4))
  expect_equal(t3$desc$target, getTaskTargetNames(t4))
  expect_equal(dim(getTaskData(t3$task)), dim(getTaskData(t4)))
  expect_equal(t3$task$task.desc$n.feat["numerics"], c(numerics = 5L))
})

test_that("Fourier equal to expected", {
  t = seq(from = 0, to = 1, length.out = 501)
  data = data.frame(a = 1:2)
  data$fd = matrix(c(
    .6 * cos(2*pi*t) + .3 * cos(4*2*pi*t + pi/4),
    .8 * cos(2*pi*t) + .1 * cos(4*2*pi*t + pi/4)
    ), nrow = 2, byrow = TRUE
  )
  data$a = NULL

  extr = extractFDAFourier()
  fourier.vals = extr$learn(data = data, col = "fd", trafo.coeff = "phase")
  fourier.gp = extr$reextract(data = data, col = "fd", vals = fourier.vals)

  # Phase in range [-180; 180]
  expect_true(all(fourier.gp >= -180 & fourier.gp <= 180))
  # ~ pi/4 shift for 4th component
  expect_true(all(abs(fourier.gp[, 5]  - 45) < 10))
  expect_true(all(abs(fourier.gp[, 2]) < 10))
  expect_true(all(dim(fourier.gp) == c(2, 501)))

  fourier.vals = extr$learn(data = data, col = "fd", trafo.coeff = "amplitude")
  fourier.gp = extr$reextract(data = data, col = "fd", vals = fourier.vals)

  # Amplitude sqrt(Re^2 + Im^2) >= 0
  expect_true(all(fourier.gp >= 0))
  expect_true(all(abs(fourier.gp[, 2]  - c(0.6, 0.8)) < 0.01))
  expect_true(all(abs(fourier.gp[, 5]  - c(0.3, 0.1)) < 0.01))
  expect_true(all(dim(fourier.gp) == c(2, 501)))

  # Can not have factors
  gp2 = data.frame(v1 = t(1:4), X1 = as.factor(1))
  expect_error(extractFourierFeatures(data = gp2, trafo.coeff = "amplitude"))
})

test_that("tsfeatures works", {
  requirePackagesOrSkip("tsfeatures")
  gp1 = getTaskData(fuelsubset.task, functionals.as = "matrix")[1:30, ]
  lrn = extractFDATsfeatures()$learn
  gpvals = lrn(data = gp1, col = "UVVIS")
  gpfeats = extractFDATsfeatures()$reextract(data = gp1, col = "UVVIS", vals = gpvals)
  expect_equal(nrow(gpfeats), nrow(gp1))

  extr = extractFDAFeatures(subsetTask(fuelsubset.task, subset = 1:30, features = 2), 
    feat.methods = list("UVVIS" = extractFDATsfeatures()))
  expect_true(ncol(getTaskData(extr$task)) == 32L)

  reextr = reextractFDAFeatures(subsetTask(fuelsubset.task, subset = 31:35, features = 2), extr$desc)
  expect_true(ncol(getTaskData(reextr)) == 32L)
})

test_that("extraction returns correct cols", {
  requirePackagesOrSkip("tsfeatures")
  extr = extractFDAFeatures(subsetTask(fuelsubset.task, subset = 1:7), 
    feat.methods = list("UVVIS" = extractFDATsfeatures()))
  reextr = reextractFDAFeatures(subsetTask(fuelsubset.task, subset = 8:14), extr$desc)
  expect_equal(extr$task$task.desc$n.feat, reextr$task.desc$n.feat)
  expect_equal(colnames(getTaskData(extr$task, functionals.as = "matrix")),
    colnames(getTaskData(reextr, functionals.as = "matrix")))
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

test_that("extractBsignal features", {
  requirePackagesOrSkip("FDboost")
  methods = list("UVVIS" = extractFDABsignal(), "NIR" = extractFDABsignal())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  t2 = reextractFDAFeatures(fuelsubset.task, t$desc)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129L)
  expect_equal(ncol(df), 30L)
})

test_that("extractFDAFeaturesDTW", {
  requirePackagesOrSkip("rucrdtw")
  methods = list("UVVIS" = extractFDADTWKernel(), "NIR" = extractFDADTWKernel())
  t = extractFDAFeatures(fuelsubset.task, feat.methods = methods)
  # check output data
  df = getTaskData(t$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129)
})
