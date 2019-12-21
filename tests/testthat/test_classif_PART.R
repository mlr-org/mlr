context("classif_PART")

test_that("classif_PART", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(M = 10),
    list(M = 5, C = 0.4),
    list(M = 5, R = TRUE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset$Q = as.integer(runif(1, min = -.Machine$integer.max,
      max = .Machine$integer.max))
    ctrl = do.call(RWeka::Weka_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = RWeka::PART(formula = multiclass.formula, data = multiclass.train,
      control = ctrl)
    p = predict(m, newdata = multiclass.test, type = "class")
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.PART", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.PART", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = function(formula, data, subset, ...) {
    RWeka::PART(formula, data = data[subset, ],
      control = RWeka::Weka_control(..., Q = as.integer(runif(1,
        min = -.Machine$integer.max, max = .Machine$integer.max))))
  }

  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCVParsets("classif.PART", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
