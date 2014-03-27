context("classif_LiblineaRBinary")

test_that("classif_LiblineaRBinary", {
  library(LiblineaR)
  
  parset.list = list(
    list(type=1),
    list(type=2),
    list(type=3),
    list(type=5)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data=binaryclass.train[, -binaryclass.class.col],
                labels=binaryclass.train[, binaryclass.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR, pars)
    p = predict(m, newx=binaryclass.test[, -binaryclass.class.col])
    old.predicts.list[[i]] = as.factor(p$predictions)
  }
  
  testSimpleParsets("classif.LiblineaRBinary", binaryclass.df, binaryclass.target,
                    binaryclass.train.inds, old.predicts.list, parset.list)
  
})
