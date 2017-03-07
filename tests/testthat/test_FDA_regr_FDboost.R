context("FDA_regr_FDboost")
# predict the heat value of fossil fuels using spectral data, one spectrum is
# ultra-violet-visible (UV-VIS), measured at 1335 wavelengths(lambda = 1/f$), the ohter a near infrared
# spectrum(NIR), measured at 2307 wavelengths(lambda = 1/f$). The distance for both data are
# not equal distance in wavelegnths.

test_that("FDA_regr_FDboost", {
    options(mlr.debug.seed = 123L)
    requirePackages("FDboost")
    data("fuelSubset", package = "FDboost")
    # center the functional covariates per observed wavelength
    fuelSubset$UVVIS = scale(fuelSubset$UVVIS, scale = FALSE)
    fuelSubset$NIR = scale(fuelSubset$NIR, scale = FALSE)
    # to make mboost:::df2lambda() happy (all design matrix entries < 10)
    # reduce range of argvals to [0,1] to get smaller integration weights
    fuelSubset$uvvis.lambda = with(fuelSubset, (uvvis.lambda - min(uvvis.lambda)) /
       (max(uvvis.lambda) - min(uvvis.lambda) ))
    fuelSubset$nir.lambda = with(fuelSubset, (nir.lambda - min(nir.lambda)) /
       (max(nir.lambda) - min(nir.lambda) ))

    len1 = length(fuelSubset$uvvis.lambda)
    len2 = length(fuelSubset$nir.lambda)
    mdata = as.data.frame(cbind(fuelSubset$UVVIS, fuelSubset$NIR, fuelSubset$h2o, fuelSubset$heatan))
    colnames(mdata)[length(colnames(mdata))] = "heatan"
    fdf = list(UVVIS = 1:len1, NIR = (len1 + 1):(len1 + len2))
    fdg = list(UVVIS = fuelSubset$uvvis.lambda, NIR = fuelSubset$nir.lambda)
    fdboost.task = makeFDARegrTask(data = mdata, target = "heatan", fd.features = fdf, fd.grids = fdg)

    mod2f = FDboost(heatan ~ bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)
   + bsignal(NIR, nir.lambda, knots = 40, df = 4, check.ident = FALSE),
    timeformula = ~bols(1), data = fuelSubset, control = boost_control(mstop = 100L))
    lrn = makeLearner("fdaregr.FDboost", bsignal.knots =40L, bsignal.df = 4L, mstop = 100L)
    mod1f = train(learner = lrn, task = fdboost.task)
    mod1f = predict(object = mod1f, newdata = mdata)
})
