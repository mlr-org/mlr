context("classif_bartMachine")

test_that("classif_bartMachine", {
  requirePackagesOrSkip("bartMachine", default.method = "load")

  parset.list = list(
    list(seed = getOption("mlr.debug.seed"), verbose = FALSE),
    list(num_burn_in = 20L, num_iterations_after_burn_in = 50L,
      run_in_sample = FALSE, seed = getOption("mlr.debug.seed"),
      verbose = FALSE),
    list(num_burn_in = 20L, num_iterations_after_burn_in = 50L, alpha = 0.8,
      num_trees = 25L, run_in_sample = FALSE, seed = getOption("mlr.debug.seed"),
      verbose = FALSE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    x = binaryclass.train
    y = factor(x[, binaryclass.class.col], levels = binaryclass.class.levs)
    x[, binaryclass.class.col] = NULL
    pars = list(X = x, y = y)
    pars = c(pars, parset)
    m = do.call(bartMachine::bartMachine, pars)
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, new_data = newx, type = "class")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, new_data = newx, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.bartMachine", binaryclass.df,
    binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.bartMachine", binaryclass.df,
    binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)

  for (i in seq_along(parset.list)) {
    expect_true(length(old.predicts.list[[i]]) == nrow(binaryclass.test))
    expect_true(length(old.probs.list[[i]]) == nrow(binaryclass.test))
  }
})
