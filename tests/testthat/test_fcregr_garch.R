context("fcregr_garch")

test_that("fcregr_garch", {
  requirePackagesOrSkip("rugarch")
  parset.list = list(
    list(spec = rugarch::ugarchspec()),
    list(spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), variance.targeting = FALSE),
      mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
        archpow = 1, arfima = FALSE, archex = FALSE),
      distribution.model = "norm", start.pars = list(), fixed.pars = list()),
      fit.control = list(stationarity = 1, fixed.se = 1, scale = 1, rec.init = "all")),
    list(spec = rugarch::ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
      submodel = "GARCH", variance.targeting = TRUE),
      mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
        archpow = 1, arfima = FALSE,  archex = FALSE),
      distribution.model = "norm", start.pars = list(), fixed.pars = list()),
      fit.control = list(stationarity = 1, fixed.se = 1, scale = 1, rec.init = "all")),
    list(spec = rugarch::ugarchspec(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = .8)),
    list( spec = rugarch::ugarchspec())
  )
  old.predicts.list = list()

  for (i in seq_len(length(parset.list))) {
    parset = parset.list[[i]]
    pars = list(data = ts(fcregr.train$test_data, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(rugarch::ugarchfit, pars)
    })
    set.seed(getOption("mlr.debug.seed"))
    garch.forecast = rugarch::ugarchforecast(m, n.ahead = 1L)
    p = as.numeric(garch.forecast@forecast$seriesFor)
    old.predicts.list[[i]] = p
  }

  # We have to reset this to fit in the model schema
  parset.list = list(
    list(n.ahead = 1L),
    list(model = "sGARCH", garchOrder = c(1, 1), variance.targeting = FALSE,
      armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
      archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE,
      distribution.model = "norm", stationarity = 1, fixed.se = 1, scale = 1,
      rec.init = "all", n.ahead = 1L),
    list(model = "fGARCH", garchOrder = c(1, 1), submodel = "GARCH",
      variance.targeting = TRUE, armaOrder = c(1, 1),
      include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE,
      external.regressors = NULL, archex = FALSE, distribution.model = "norm",
      stationarity = 1, fixed.se = 1, scale = 1,
      rec.init = "all", n.ahead = 1L),
    list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = .8, n.ahead = 1L),
    list(n.ahead = 1L)
  )
  testSimpleParsets("fcregr.garch", fcregr.df, fcregr.target,
    fcregr.train.inds, old.predicts.list, parset.list)
})
