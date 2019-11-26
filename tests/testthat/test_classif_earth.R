context("classif_earth")

test_that("classif_earth can do binary classification", {
  requirePackagesOrSkip("earth", default.method = "load")

  parset.list = list(
    list(),
    list(degree = 3L, nprune = 2L),
    list(penalty = 2, nk = 2L, thresh = 0.1),
    list(fast.k = 2L, fast.beta = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  levs = binaryclass.class.levs
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset$glm = list(family = binomial(link = "logit"), maxit = 25)
    pars = list(formula = binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    # suppressed warnings: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
    m = suppressWarnings(do.call(earth::earth, pars))
    p = predict(m, newdata = binaryclass.test[, -binaryclass.class.col], type = "response")[, 1]
    old.probs.list[[i]] = 1 - p
    old.predicts.list[[i]] = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
  }

  # suppressed warnings: "the glm algorithm did not converge for response "R"
  # glm.fit: algorithm did not converge
  suppressWarnings(
    testSimpleParsets("classif.earth", binaryclass.df, binaryclass.target,
      binaryclass.train.inds, old.predicts.list, parset.list)
  )
  suppressWarnings(
    testProbParsets("classif.earth", binaryclass.df, binaryclass.target,
      binaryclass.train.inds, old.probs.list, parset.list)
  )
})

test_that("classif_earth can do multiclass classification", {
  requirePackagesOrSkip("earth", default.method = "load")

  parset.list = list(
    list(),
    list(degree = 3L, nprune = 2L),
    list(penalty = 2, nk = 2L, thresh = 0.1),
    list(fast.k = 2L, fast.beta = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset$glm = list(family = binomial(link = "logit"), maxit = 25)
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    # suppressed warnings: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
    m = suppressWarnings(do.call(earth::earth, pars))
    p = predict(m, newdata = multiclass.test[, -multiclass.class.col], type = "response")
    old.probs.list[[i]] = p
    old.predicts.list[[i]] = as.factor(predict(m,
      newdata = multiclass.test[, -multiclass.class.col], type = "class"))
  }

  # suppressed warnings: " warning: classif_earth can do multiclass classification
  # glm.fit: algorithm did not converge"
  suppressWarnings(
    testSimpleParsets("classif.earth", multiclass.df, multiclass.target,
      multiclass.train.inds, old.predicts.list, parset.list)
  )
  suppressWarnings(
    testProbParsets("classif.earth", multiclass.df, multiclass.target,
      multiclass.train.inds, old.probs.list, parset.list)
  )
})
