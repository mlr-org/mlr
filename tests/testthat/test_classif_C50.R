context("classif_C50")

test_that("classif_C50", {
  # test for issue #1573: make C50 learner objects not stochastic
  lrn1 = makeLearner("classif.C50")
  lrn2 = makeLearner("classif.C50")
  expect_equal(lrn1, lrn2)

  requirePackages("C50", default.method = "load")

  parset.list = list(
    list(),
    list(control = C50::C5.0Control(), seed = getOption("mlr.debug.seed")),
    list(trials = 5L, control = C50::C5.0Control(minCases = 10L, seed = getOption("mlr.debug.seed"))),
    list(rules = TRUE, control = C50::C5.0Control(bands = 100L, seed = getOption("mlr.debug.seed"))),
    list(control = C50::C5.0Control(CF = 0.5, sample = 0.444, seed = getOption("mlr.debug.seed")))
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(x = multiclass.train[, names(multiclass.train) %nin% multiclass.target],
      y = multiclass.train[, multiclass.target])
    pars = c(pars, parset)
    m = do.call(C50::C5.0, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test, type = "class")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  parset.list = list(
    list(),
    list(seed = getOption("mlr.debug.seed")),
    list(trials = 5L, minCases = 10L, seed = getOption("mlr.debug.seed")),
    list(rules = TRUE, bands = 100L, seed = getOption("mlr.debug.seed")),
    list(CF = 0.5, sample = 0.444, seed = getOption("mlr.debug.seed"))
  )

  testSimpleParsets("classif.C50", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.C50", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)
})
