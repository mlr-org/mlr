context("classif_adaboostM1")

test_that("classif_adaboostM1", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(P = 100),
    list(I = 10, S = 1),
    list(I = 5, Q= FALSE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::AdaBoostM1(formula = multiclass.formula, data = multiclass.train, control = ctrl)
    p  = predict(m, newdata = multiclass.test, type = "class")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.adaboostM1", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.adaboostM1", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset, ...) {
    RWeka::AdaBoostM1(formula, data = data[subset, ], control = RWeka::Weka_control(...))
  }

  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.adaboostM1", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp, parset.list = parset.list)

})

