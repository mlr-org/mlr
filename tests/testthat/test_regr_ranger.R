context("regr_ranger")

## Suppress 'experimental' warning for splirule = maxstat 
test_that("regr_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2),
    list(num.trees = 10L, splitrule = "maxstat", alpha = 0.25),
    list(num.trees = 10L, splitrule = "maxstat", minprop = 0.25)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, write.forest = TRUE, respect.unordered.factors = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = suppressWarnings(do.call(ranger::ranger, parset))
    p  = predict(m, data = regr.test)
    old.predicts.list[[i]] = p$predictions
  }

  suppressWarnings(testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list))
})
