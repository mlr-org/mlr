context("fcregr_auto.arima")

test_that("fcregr_auto.arima", {

  parset.list = list(
    list(),
    list(start.p = 4),
    list(max.q = 3),
    list(seasonal.test = "ch", allowmean = FALSE),
    list(start.Q = 4, seasonal = FALSE, max.order = 6)
  )
  old.predicts.list = list()

  for (i in seq_len(length(parset.list))) {
    parset = parset.list[[i]]
    pars = list(y = ts(fcregr.train, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::auto.arima, pars)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = as.numeric(forecast::forecast(m, h = 1L)$mean)
    old.predicts.list[[i]] = p
  }

  parset.list[[1]]$h = 1L
  parset.list[[2]]$h = 1L
  parset.list[[3]]$h = 1L
  parset.list[[4]]$h = 1L
  parset.list[[5]]$h = 1L
  testSimpleParsets("fcregr.auto.arima", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})


test_that("fcregr_auto.arima_update", {
  parset.list = list(
    list(),
    list(max.p = 8),
    list(stationary = TRUE, max.order = 4),
    list(seasonal = FALSE),
    list(allowmean = FALSE, max.d = 3)
  )
  old.predicts.list = list()

  for (i in seq_len(length(parset.list))) {
    parset = parset.list[[i]]
    pars = list(y = as.ts(fcregr.update.train))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::auto.arima, pars)
    })
    parset$model = m
    pars.update = list(y = as.ts(fcregr.update.update), model = m)
    #pars.update = c(pars.update,parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::Arima, pars.update)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = as.numeric(forecast::forecast(m, h = 1L)$mean)
    old.predicts.list[[i]] = p
  }

  parset.list[[1]]$h = 1L
  parset.list[[2]]$h = 1L
  parset.list[[3]]$h = 1L
  parset.list[[4]]$h = 1L
  parset.list[[5]]$h = 1L
  testSimpleParsetsUpdate("fcregr.auto.arima", fcregr.update.xts, fcregr.target,
                          fcregr.update.update.inds, fcregr.update.train.inds,
                          fcregr.update.test.inds, old.predicts.list, parset.list)
})
