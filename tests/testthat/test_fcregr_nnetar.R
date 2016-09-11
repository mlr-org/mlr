context("fcregr_nnetar")

test_that("fcregr_nnetar", {

  parset.list = list(
    list(),
    list(p = 3),
    list(p = 2, size = 4),
    list(scale.inputs = FALSE, p = 3),
    list(Hess = TRUE, size = 3, decay = .01)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = ts(fcregr.train, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::nnetar, pars)
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
  testSimpleParsets("fcregr.nnetar", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})

test_that("fcregr_nnetar_update",{
  parset.list = list(
    list(),
    list(p = 3),
    list(p = 2, size = 4),
    list(scale.inputs = FALSE, p = 3),
    list(Hess = TRUE, size = 3, decay = .01)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = ts(zoo::coredata(fcregr.update.train), frequency = 1))
    frequency(pars$y)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::nnetar, pars)
    })
    parset$model = m
    pars.update = list(y = ts(zoo::coredata(fcregr.update.update), frequency = 1))
    pars.update = c(pars.update,parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::nnetar, pars.update)
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
  testSimpleParsetsUpdate("fcregr.nnetar", fcregr.update.xts, fcregr.target,
                          fcregr.update.update.inds, fcregr.update.train.inds,
                          fcregr.update.test.inds, old.predicts.list, parset.list)
})
