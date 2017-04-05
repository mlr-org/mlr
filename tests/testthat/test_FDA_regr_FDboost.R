context("FDA_regr_FDboost")
# predict the heat value of fossil fuels using spectral data, one spectrum is
# ultra-violet-visible (UV-VIS), measured at 1335 wavelengths(lambda = 1/f$), the ohter a near infrared
# spectrum(NIR), measured at 2307 wavelengths(lambda = 1/f$). The distance for both data are
# not equal distance in wavelegnths.

test_that("FDA_regr_FDboost", {
    options(mlr.debug.seed = 123L)
    requirePackages("FDboost")
    lrn = makeLearner("fdaregr.FDboost", knots = 40L, df = 4L, mstop = 100L)
    mod1f = train(learner = lrn, task = fuelsubset.task)
    mdata = getTaskData(fuelsubset.task, target.extra = TRUE)
    mod1f = predict(object = mod1f, newdata = mdata$data)
})
