context("multilabel_cforest")

test_that("multilabel_cforest", {
  requirePackagesOrSkip("party", default.method = "load")

  parset.list = list(
    list(),
    list(control = party::cforest_unbiased(mtry = 2)),
    list(control = party::cforest_unbiased(ntree = 200))
  )
  parset.list2 = list(
    list(),
    list(mtry = 2),
    list(ntree = 200)
  )

  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(multilabel.formula, data = multilabel.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(party::cforest, pars)
    # multivariate cforest can only predict probs
    p = predict(m, newdata = multilabel.test)
    p2 = do.call(rbind, p)
    old.probs.list[[i]] = data.frame(p2)
  }

  testProbParsets("multilabel.cforest", multilabel.df, multilabel.target,
    multilabel.train.inds, old.probs.list, parset.list2)
})
