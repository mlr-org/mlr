context("classif_avNNet")

test_that("classif_avNNet", {
  requirePackagesOrSkip("nnet", default.method = "load")
  
  parset.list1 = list(
    list(size = 3),
    list(size = 6)
  )
  
  parset.list2 = list(
    list(),
    list(size = 6)
  )

  old.predicts.list = list()
  
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    repeats = 5
    pred = 0
    for (j in 1:repeats) {
      m = do.call(nnet::nnet, pars)
      pred = pred+predict(m, binaryclass.test)
    }
    pred = pred/repeats
    old.predicts.list[[i]] = factor(as.numeric(pred>0.5), labels = binaryclass.class.levs)
  }

  testSimpleParsets("classif.avNNet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
})
