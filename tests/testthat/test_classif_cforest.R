context("classif_cforest")

test_that("classif_cforest", {
  requirePackagesOrSkip("partykit", default.method = "load")

  parset.list = list(
    list(),
    list(mtry = 2),
    list(ntree = 200)
  )
  parset.list2 = list(
    list(),
    list(mtry = 2),
    list(ntree = 200)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(partykit::cforest, pars)
    old.predicts.list[[i]] = predict(m, newdata = binaryclass.test)
    old.probs.list[[i]] = predict(m, newdata = binaryclass.test, type = "prob")[, 1]
  }

  testSimpleParsets("classif.cforest", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list2)
  testProbParsets("classif.cforest", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list2)

  # issue 556
  parset.list3 = list(
    list(perturb.replace = FALSE)
  )
  testSimpleParsets("classif.cforest", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list3)
})
