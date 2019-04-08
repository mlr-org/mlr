
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
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(earth::earth, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test[, -binaryclass.class.col], type = "response")[, 1]
    old.probs.list[[i]] = 1 - p
    old.predicts.list[[i]] = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
  }

  testSimpleParsets("classif.earth", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.earth", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
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
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(earth::earth, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test[, -multiclass.class.col], type = "response")
    old.probs.list[[i]] = p
    old.predicts.list[[i]] = as.factor(predict(m, newdata = multiclass.test[, -multiclass.class.col], type = "class"))
  }

  testSimpleParsets("classif.earth", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.earth", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)
})

