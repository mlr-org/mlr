context("classif_JRip")

test_that("classif_JRip", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(F = 5),
    list(F = 4, N = 3),
    list(F = 2, O = 4)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    parset$S = as.integer(runif(1, min = -.Machine$integer.max, max = .Machine$integer.max))
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::JRip(formula = multiclass.formula, data = multiclass.train, control = ctrl)
    p = predict(m, newdata = multiclass.test, type = "class")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.JRip", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.JRip", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset, ...) {
    RWeka::JRip(formula, data = data[subset, ],
      control = RWeka::Weka_control(..., S = as.integer(runif(1,
        min = -.Machine$integer.max, max = .Machine$integer.max))))
  }

  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.JRip", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
