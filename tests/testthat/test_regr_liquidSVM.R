context("regr_liquidSVM")

test_that("regr_liquidSVM", {
  requirePackagesOrSkip("liquidSVM", default.method = "load")
  parset.list = list(
    list(),
    list(partition_choice = 6),
    list(partition_choice = 5),
    list(grid_choice = 1),
    list(grid_choice = 2),
    list(adaptivity_control = 1),
    list(adaptivity_control = 2),
    list(clipping = -1),
    list(clipping = 0),
    list(gamma_steps = 5, min_gamma = 0.1, max_gamma = 25, lambda_steps = 5, min_lambda = 0.1, max_lambda = 25),
    list(useCells = TRUE),
    list(gammas = c(0.1, 1, 10), lambdas = c(0.1, 1, 10), c_values = c(0.1, 1, 10))
  )

  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(x = regr.formula, y = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(liquidSVM::svm, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.liquidSVM", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
