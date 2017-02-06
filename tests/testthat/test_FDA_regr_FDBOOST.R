context("FDA_regr_FDboost")

#' predict the heat value of fossil fuels using spectral data, one spectrum is
#' ultra-violet-visible (UV-VIS), measured at 1335 wavelengths(lambda = 1/f$), the ohter a near infrared
#' spectrum(NIR), measured at 2307 wavelengths(lambda = 1/f$). The distance for both data are
#' not equal distance in wavelegnths.
test_that("FDA_regr_FDboost", {

#   data("fuelSubset", package = "FDboost")
#   ## center the functional covariates per observed wavelength
#   fuelSubset$UVVIS <- scale(fuelSubset$UVVIS, scale = FALSE)
#   fuelSubset$NIR <- scale(fuelSubset$NIR, scale = FALSE)
#   ## to make mboost:::df2lambda() happy (all design matrix entries < 10)
#   ## reduce range of argvals to [0,1] to get smaller integration weights
#   fuelSubset$uvvis.lambda <- with(fuelSubset, (uvvis.lambda - min(uvvis.lambda)) /
#       (max(uvvis.lambda) - min(uvvis.lambda) ))
#   fuelSubset$nir.lambda <- with(fuelSubset, (nir.lambda - min(nir.lambda)) /
#       (max(nir.lambda) - min(nir.lambda) ))
#
#   ############################
#   dim1 = dim(fuelSubset$UVVIS)
#   LEN1 = length(fuelSubset$uvvis.lambda)
#   dim2 = dim(fuelSubset$NIR)
#   length(fuelSubset$heatan)
#   length(fuelSubset$h2o)
#   LEN2 = length(fuelSubset$nir.lambda)
#   data = as.data.frame(Reduce(cbind, list(fuelSubset$UVVIS, fuelSubset$NIR, fuelSubset$h2o, fuelSubset$heatan)))
#   colnames(data)[length(colnames(data))] = "heat"
#   channel.list = list(UVVIS = 1:LEN1, NIR = (LEN1 + 1):(LEN1 + LEN2 + 1))
#   index.list = list(UVVIS = fuelSubset$uvvis.lambda, NIR = fuelSubset$nir.lambda)
#   reg.task = makeTimeSeriesRegrTask(data = data, target = "heat", channel.list = channel.list, formula.list = NULL, index.list = index.list)
#
# ## possibility 1: as FLAM model with the scalar response
# ## adds a penalty over the index of the response
# ## thus, mod2f and mod2 have different panlties
# mod2f <- FDboost(heatan ~ bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)
#   + bsignal(NIR, nir.lambda, knots = 40, df = 4, check.ident = FALSE),
#   timeformula = ~bols(1), data = fuelSubset, control = boost_control(mstop = 200))
#
# ## possibility 2: with scalar response
# mod2 <- FDboost(heatan ~ bsignal(UVVIS, uvvis.lambda, knots = 40, df = 4, check.ident = FALSE)
#   + bsignal(NIR, nir.lambda, knots = 40, df = 4, check.ident = FALSE),
#   timeformula = NULL, data = fuelSubset, control = boost_control(mstop = 200))
# ## Not run:
# ## bootstrap to find optimal mstop takes some time
# set.seed(123)
# folds2 <- cv(weights = model.weights(mod2), B = 10)
# cvm2 <- cvrisk(mod2, folds = folds2, grid = 1:1000)
# mstop(cvm2) ## mod2[327]
# summary(mod2)
# ## plot(mod2)

## End(Not run)
})
