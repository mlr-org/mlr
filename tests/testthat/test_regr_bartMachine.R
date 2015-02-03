context("regr_bartMachine")

test_that("regr_bartMachine", {
  requirePackages("bartMachine", default.method = "load")
  parset.list = list(
    list(num_burn_in = 20L, num_iterations_after_burn_in = 50L, run_in_sample = FALSE),
    list(num_burn_in = 20L, num_iterations_after_burn_in = 50L, alpha = 0.8, num_trees = 25L,
      run_in_sample = FALSE)
  )

  old.predicts.list = list()
  xind = names(regr.train) != regr.target

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = regr.train[, regr.target], X = regr.train[, xind], verbose = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(bartMachine::bartMachine, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, new_data = regr.test[, xind])
  }

  # FIXME:
  # Does not yet work because we can not yet set the seed for bartMachine, see
  # https://github.com/kapelner/bartMachine/issues/2
  # testSimpleParsets("regr.bartMachine", regr.df, regr.target, regr.train.inds,
  #  old.predicts.list, parset.list)

  for(i in 1:length(parset.list)){
    expect_true(length(old.predicts.list[[i]]) == nrow(regr.test))
  }
})
