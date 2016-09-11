context("mfcregr_BigVAR")



test_that("mfcregr_BigVAR", {

  parset.list = list(
    # This is as close to empty as it can be
    list(p = 2, struct = "Basic", gran = c(2, 4),
      h = 1),
    list(p = 2, struct = "Lag", gran = c(5, 2),
      h = 1),
    list(p = 6, struct = "SparseOO", gran = c(3, 4),
      h = 1,  RVAR = TRUE),
    list(p = 3, struct = "HVARC", gran = c(5, 5),
      h = 1,  MN = TRUE),
    list(p = 6, struct = "Tapered", gran = c(4, 4),
      h = 1,  ONESE = TRUE)

  )
  old.predicts.list = list()

  for (i in seq_len(length(parset.list))) {
    parset = parset.list[[i]]
    pars = list(Y = as.matrix(mfcregr.train[, -5]))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      obj = do.call(BigVAR::constructModel, pars)
    })
    capture.output({
      m = BigVAR::cv.BigVAR(obj)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = BigVAR::predict(m, n.ahead = 1L)
    p = as.data.frame(t(p))
    colnames(p) = colnames(mfcregr.train)[1:4]
    #rownames(p) = as.character(rownames(p))
    old.predicts.list[[i]] = p
  }

  for (i in seq_len(length(parset.list))) {
    parset.list[[i]]$n.ahead = 1L
    parset.list[[i]] = list(par.vals = parset.list[[i]])
    parset.list[[i]]$predict.type = "response"
    parset.list[[i]]$predict.threshold = NULL
  }
  testSimpleParsets("mfcregr.BigVAR", mfcregr.df, mfcregr.target,
    mfcregr.train.inds, old.predicts.list, parset.list)
})
