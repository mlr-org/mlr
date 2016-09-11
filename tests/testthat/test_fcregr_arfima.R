context("fcregr_arfima")

test_that("fcregr_arfima", {

  parset.list = list(
    list(),
    list(drange = c(0, .5), lambda=numeric(1)),
    list(max.p = 5, max.q = 5, max.d = 3, start.p = 1, lambda=numeric(1)),
    list(max.p = 2, start.p = 1, seasonal = FALSE, lambda=numeric(1)),
    list(biasadj = TRUE, allowdrift = FALSE, allowmean = FALSE, lambda=numeric(1))
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    # NOTE: This function only accepts positive values
    pars = list(y = ts(abs(fcregr.train), start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::arfima, pars)
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
  testSimpleParsets("fcregr.arfima", abs(fcregr.xts), fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})
