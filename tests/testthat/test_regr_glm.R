context("regr_glm")

test_that("regr_glm", {
  requirePackagesOrSkip("glm", default.method = "load")
  
  parset.list = list(
    list(),
    list(family = inverse.gaussian(link = "log"), start = rep(0.4,14)),
    list(trace = TRUE, epsilon = 1e-10, maxit = 30),
    list(x = TRUE, y = FALSE)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(stats::glm, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }
  
  # names for family and link in mlr differ from glm() argument family
  parset.list[[2]]$family = "inverse.gaussian"
  parset.list[[2]]$inverse.gaussian.link = "log"
  
  testSimpleParsets("regr.glm", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)
  
})


