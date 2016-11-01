context("fcregr_Arima")



test_that("fcregr_Arima", {

  parset.list = list(
    list(),
    list(order = c(2,0,1)),
    list(order = c(2,0,1), include.mean = TRUE),
    list(order = c(2,0,2), include.mean = TRUE, include.drift = TRUE),
    list(order = c(2,0,1), method = c("ML"))

  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = fcregr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::Arima, pars)
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
  testSimpleParsets("fcregr.Arima", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})


test_that("fcregr_Arima_update",{
  parset.list = list(
    list(),
    list(order = c(2,0,1)),
    list(order = c(2,0,1), include.mean = TRUE),
    # FIXME: Why does this fail for include.drift = TRUE???
    list(order = c(2,0,2), include.mean = TRUE, method = c("ML")),
    list(order = c(2,0,1), method = c("ML"))

  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = as.ts(fcregr.update.train))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::Arima, pars)
    })
    parset$model = m
    pars.update = list(y = as.ts(fcregr.update.update))
    pars.update = c(pars.update,parset)
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
  testSimpleParsetsUpdate("fcregr.Arima", fcregr.update.xts, fcregr.target,
                          fcregr.update.update.inds, fcregr.update.train.inds,
                          fcregr.update.test.inds, old.predicts.list, parset.list)
})


