context("regr_avNNet")

test_that("regr_avNNet", {
  requirePackagesOrSkip("nnet", default.method = "load")
  
  parset.list1 = list(
    list(),
    list(size = 7L, linout = TRUE)
  )
  
  parset.list2 = list(
    list(),
    list(size = 7L, linout = TRUE)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    if (is.null(parset$size)) parset$size = 3L
    if (is.null(parset$linout)) parset$linout = TRUE
    pars = list(formula = regr.formula, data = regr.train)
    pars = c (pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    repeats = 5
    pred = 0
    for (j in 1:repeats) {
      model = do.call(nnet::nnet, pars)
      pred = pred + predict(model, regr.test)
    }
    old.predicts.list[[i]] = as.vector(pred/repeats)
  }

  #set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("regr.avNNet", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list2)
})
