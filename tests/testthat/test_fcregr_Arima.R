context("fcregr_Arima")



test_that("fcregr_Arima", {

  parset.list = list(
    list(),
    list(order = c(2, 0, 1)),
    list(order = c(2, 0, 1), include.mean = TRUE),
    list(order = c(2, 0, 2), include.mean = TRUE, include.drift = TRUE),
    list(order = c(2, 0, 1), method = "ML")

  )
  old.predicts.list = list()

  for (i in seq_len(length(parset.list))) {
    parset = parset.list[[i]]
    pars = list(y = fcregr.train$test_data)
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
  testSimpleParsets("fcregr.Arima", fcregr.df, fcregr.target,
    fcregr.train.inds, old.predicts.list, parset.list)
})



