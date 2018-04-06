context("surv_cforest")

test_that("surv_cforest", {
  requirePackagesOrSkip(c("partykit", "!survival"), default.method = "load")

  parset.list = list(
    list(),
    list(mtry = 2),
    list(ntree = 50)
  )
  parset.list2 = list(
    list(),
    list(mtry = 2),
    list(ntree = 50)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(surv.formula, data = surv.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(partykit::cforest, pars)
    old.predicts.list[[i]] = -1 * predict(m, newdata = surv.test)
  }

  testSimpleParsets("surv.cforest", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list2)

  # issue 556
  parset.list3 = list(
    list(perturb.replace = FALSE)
  )
  testSimpleParsets("surv.cforest", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list3)
})
