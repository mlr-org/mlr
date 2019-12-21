context("regr_cforest")

test_that("regr_cforest", {
  requirePackagesOrSkip("party", default.method = "load")

  parset.list = list(
    list(),
    list(control = party::cforest_unbiased(mtry = 2, ntree = 50)),
    list(control = party::cforest_unbiased(ntree = 50))
  )
  parset.list2 = list(
    list(),
    list(mtry = 2, ntree = 50),
    list(ntree = 50)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(party::cforest, pars)
    old.predicts.list[[i]] = as.vector(predict(m, newdata = regr.test))
  }

  testSimpleParsets("regr.cforest", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list2)

  # issue 556
  parset.list3 = list(
    list(replace = FALSE)
  )
  testSimpleParsets("regr.cforest", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list3)
})
