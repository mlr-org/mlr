test_that("regr_gstat", {
  requirePackagesOrSkip("gstat", default.method = "load")

  parset.list = list(
    list(),
    list(id = "trend_surfaces_degree_1", degree = 1, locations = ~x+y),
    list(id = "trend_surfaces_degree_2", degree = 2, locations = ~x+y),
    list(id = "trend_surfaces_degree_3", degree = 3, locations = ~x+y),
    list(id = "inverse_distance_weighted", locations = ~x+y),
    list(id = "ordinary_kriging", predict.type = "response",
      model = list(psill = 1, model = "Sph", range = 900, nugget = 1), locations = ~x+y),
    list(id = "universal_kriging", predict.type = "response",
      model = list(psill = 1, model = "Exp", range = 300, nugget = 1), locations = ~x+y)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(gstat::gstat, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.train, type = "response")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.gstat", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)


})
