context("regr_bartMachine")

test_that("regr_bartMachine", {
  requirePackagesOrSkip("bartMachine", default.method = "load")

  parset.list = list(
    list(num_burn_in = 10L, num_iterations_after_burn_in = 10L,
      run_in_sample = FALSE, seed = getOption("mlr.debug.seed")),
    list(num_burn_in = 10L, num_iterations_after_burn_in = 10L, alpha = 0.8,
      num_trees = 25L, run_in_sample = FALSE,
      seed = getOption("mlr.debug.seed"))
  )

  old.predicts.list = list()
  xind = names(regr.train) != regr.target

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = regr.train[, regr.target], X = regr.train[, xind],
      verbose = FALSE)
    pars = c(pars, parset)
    m = do.call(bartMachine::bartMachine, pars)
    old.predicts.list[[i]] = predict(m, new_data = regr.test[, xind])
  }

  testSimpleParsets("regr.bartMachine", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)

  for (i in seq_along(parset.list)) {
    expect_true(length(old.predicts.list[[i]]) == nrow(regr.test))
  }
})
