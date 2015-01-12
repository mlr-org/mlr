context("classif_LiblineaRLogReg")

test_that("classif_LiblineaRLogReg", {
  requirePackages("LiblineaR")

  # parset.list = list(
  #   list(type = 0),
  #   list(type = 6),
  #   list(type = 7)
  # )

  # old.predicts.list = list()
  # old.probs.list = list()

  # FIXME: I think we cannot seed liblinear (C code wrong?
  # so we just run the algo
  
  
  for (type in c(0, 6, 7)) {
    lrn = makeLearner("classif.LiblineaRLogReg", type = type)
    mod = train(lrn, binaryclass.task)
    pred = predict(mod, binaryclass.task)
    p = performance(pred)
    expect_true(!is.na(p))
  }
  
  # for (i in 1:length(parset.list)) {
  #   parset = parset.list[[i]]
  #   pars = list(data = binaryclass.train[, -binaryclass.class.col],
  #     labels = binaryclass.train[, binaryclass.target])
  #   pars = c(pars, parset)
  #   set.seed(getOption("mlr.debug.seed"))
  #   m = do.call(LiblineaR, pars)
  #   set.seed(getOption("mlr.debug.seed"))
  #   p = predict(m, newx = binaryclass.test[, -binaryclass.class.col], proba = TRUE)
  #   print(str(p))
  #   old.predicts.list[[i]] = as.factor(p$predictions)
  #   old.probs.list[[i]] = p$probabilities[, 2]
  # }

  # testSimpleParsets("classif.LiblineaRLogReg", binaryclass.df, binaryclass.target,
  #   binaryclass.train.inds, old.predicts.list, parset.list)
  # testProbParsets ("classif.LiblineaRLogReg", binaryclass.df, binaryclass.target,
  #   binaryclass.train.inds, old.probs.list, parset.list)

})
