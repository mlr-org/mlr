context("classif_ctree")

test_that("classif_ctree", {
  requirePackagesOrSkip("party", default.method = "load")

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
    ctrl = do.call(party::ctree_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = party::ctree(formula = multiclass.formula, data = multiclass.train, controls = ctrl)
    p = predict(m, newdata = multiclass.test, type = "response")
    p2 = Reduce(rbind, party::treeresponse(m, newdata = multiclass.test, type = "prob"))
    rownames(p2) = NULL
    colnames(p2) = levels(multiclass.df[, multiclass.target])
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.ctree", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.ctree", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  df = iris
  df[, 1] = 1:150
  df1 = df[seq(1, 150, 2), ]
  df2 = df[seq(2, 150, 2), ]
  ct = makeClassifTask(target = "Species", data = df1)
  m = train(makeLearner("classif.ctree"), ct)
  predict(m, newdata = df2)
})
