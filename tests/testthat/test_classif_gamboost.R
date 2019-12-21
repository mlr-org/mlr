context("classif_gamboost")

test_that("classif_gamboost", {
  requirePackagesOrSkip("mboost", default.method = "attach")

  parset.list1 = list(
    list(family = mboost::Binomial()),
    list(family = mboost::Binomial(), baselearner = "bols",
      control = mboost::boost_control(nu = 0.03, mstop = 10)),
    list(family = mboost::Binomial(link = "probit"), baselearner = "btree",
      control = mboost::boost_control(mstop = 10))
  )

  parset.list2 = list(
    list(),
    list(family = "Binomial", baselearner = "bols", nu = 0.03, mstop = 10),
    list(family = "Binomial", Binomial.link = "probit", baselearner = "btree", mstop = 10)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    m = do.call(mboost::gamboost, pars)

    # suppressed warnings: "Some ‘x’ values are beyond ‘boundary.knots’; Linear extrapolation used."
    suppressWarnings(
      old.predicts.list[[i]] <- predict(m, newdata = binaryclass.test, type = "class")
    )
    suppressWarnings(
      old.probs.list[[i]] <- 1 - predict(m, newdata = binaryclass.test, type = "response")[, 1]
    )
  }

  # suppressed warnings: "Some ‘x’ values are beyond ‘boundary.knots’; Linear extrapolation used."
  suppressWarnings(
    testSimpleParsets("classif.gamboost", binaryclass.df, binaryclass.target,
      binaryclass.train.inds, old.predicts.list, parset.list2)
  )
  suppressWarnings(
    testProbParsets("classif.gamboost", binaryclass.df, binaryclass.target,
      binaryclass.train.inds, old.probs.list, parset.list2)
  )
})

test_that("classif_gamboost probability predictions with family 'AUC' and 'AdaExp'", {
  families = list("AUC", "AdaExp")
  lapply(families, FUN = function(x) {
    lrn = makeLearner("classif.gamboost", par.vals = list(family = x), predict.type = "prob")
    mod = train(lrn, binaryclass.task)
    expect_error(predict(mod, binaryclass.task), "support probabilities")
  })
})
