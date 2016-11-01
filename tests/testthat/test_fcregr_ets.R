context("fcregr_ets")

test_that("fcregr_ets", {

  parset.list = list(
    list(),
    list( model = "ANN"),
    list( model = "ZAZ", ic = "bic"),
    list(opt.crit = "amse", bounds = "usual"),
    list( model = "AZZ", lambda = 1)
  )
  old.predicts.list = list()
  #FIXME: Have to load the namespace?
  requireNamespace("forecast")
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = ts(fcregr.train, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::ets, pars)
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
  testSimpleParsets("fcregr.ets", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})

test_that("fcregr_ets_update",{
  parset.list = list(
    list(),
    list( model = "ANN"),
    list( model = "ZAZ", ic = "bic"),
    list(opt.crit = "amse", bounds = "usual"),
    list( model = "AZZ", lambda = 1)
  )
  old.predicts.list = list()
  #FIXME: Have to load the namespace?
  library("forecast")
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = as.ts(fcregr.update.train))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::ets, pars)
    })
    parset$model = m
    pars.update = list(y = as.ts(fcregr.update.update))
    pars.update = c(pars.update,parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::ets, pars.update)
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
  testSimpleParsetsUpdate("fcregr.ets", fcregr.update.xts, fcregr.target,
                          fcregr.update.update.inds, fcregr.update.train.inds,
                          fcregr.update.test.inds, old.predicts.list, parset.list)
})
