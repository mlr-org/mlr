context("classif_LiblineaRBinary")

test_that("classif_LiblineaRBinary", {
  library(LiblineaR)

  # parset.list = list(
  #   list(type = 1),
  #   list(type = 2),
  #   list(type = 3),
  #   list(type = 5)
  # )

  # FIXME: I think we cannot seed liblinear (C code wrong?
  # so we just run the algo

  for (type in c(1, 2, 3, 5)) {
    lrn = makeLearner("classif.LiblineaRBinary", type = type)
    mod = train(lrn, binaryclass.task)
    pred = predict(mod, binaryclass.task)
    p = performance(pred)
    expect_true(!is.na(p))
  }

  # old.predicts.list = list()

  # for (i in 1:length(parset.list)) {
  #   parset = parset.list[[i]]
  #   pars = list(data = binaryclass.train[, -binaryclass.class.col],
  #               labels = binaryclass.train[, binaryclass.target])
  #   pars = c(pars, parset)
  #   set.seed(getOption("mlr.debug.seed"))
  #   m = do.call(LiblineaR, pars)
  #   p = predict(m, newx = binaryclass.test[, -binaryclass.class.col])
  #   old.predicts.list[[i]] = as.factor(p$predictions)
  # }

  # testSimpleParsets("classif.LiblineaRBinary", binaryclass.df, binaryclass.target,
  #                   binaryclass.train.inds, old.predicts.list, parset.list)

})
