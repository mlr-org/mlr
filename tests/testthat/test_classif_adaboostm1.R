context("classif_adaboostm1")

test_that("classif_adaboostm1", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(W = list(RWeka::J48, M = 30)),
    list(P = 100),
    list(I = 10, S = 1),
    list(I = 5, Q = FALSE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::AdaBoostM1(formula = binaryclass.formula,
      data = binaryclass.train, control = ctrl)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test, type = "probability")
    old.probs.list[[i]] = p[, 1]
    old.predicts.list[[i]] = as.factor(binaryclass.class.levs[ifelse(p[, 2] > 0.5, 2, 1)])
  }

  testSimpleParsets("classif.adaboostm1", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)

  testProbParsets("classif.adaboostm1", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::AdaBoostM1(formula = multiclass.formula, data = multiclass.train,
      control = ctrl)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test, type = "class")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2

  }

  testSimpleParsets("classif.adaboostm1", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)

  testProbParsets("classif.adaboostm1", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset, ...) {
    RWeka::AdaBoostM1(formula, data = data[subset, ],
      control = RWeka::Weka_control(...))
  }

  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.adaboostm1", multiclass.df, multiclass.target, #
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
