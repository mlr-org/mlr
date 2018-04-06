context("classif_ctree")

test_that("classif_ctree", {
  requirePackagesOrSkip("partykit", default.method = "load")

  parset.list = list(
    list(),
    list(minsplit = 10, mincriterion = 0.005),
    list(minsplit = 50, mincriterion = 0.05),
    list(minsplit = 50, mincriterion = 0.999),
    list(minsplit = 1, mincriterion = 0.0005)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(partykit::ctree_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = partykit::ctree(formula = multiclass.formula, data = multiclass.train, control = ctrl)
    p  = predict(m, newdata = multiclass.test, type = "response")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    rownames(p2) = NULL
    colnames(p2) = levels(multiclass.df[, multiclass.target])
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.ctree", multiclass.df, multiclass.target, multiclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.ctree", multiclass.df, multiclass.target, multiclass.train.inds,
    old.probs.list, parset.list)

})
