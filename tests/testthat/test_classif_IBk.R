context("classif_IBk")

test_that("classif_IBk", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(K = 2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = RWeka::IBk(formula = multiclass.formula, data = multiclass.train, control = ctrl)
    p = predict(m, newdata = multiclass.test, type = "class")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.IBk", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.IBk", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset, ...) {
    RWeka::IBk(formula, data = data[subset, ], control = RWeka::Weka_control(...))
  }

  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.IBk", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
