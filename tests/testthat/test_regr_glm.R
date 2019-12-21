context("regr_glm")

test_that("regr_glm", {
  parset.list = list(
    list(),
    list(trace = TRUE, epsilon = 1e-10, maxit = 10),
    list(x = TRUE, y = FALSE),
    list(family = Gamma(link = "inverse")),
    list(family = gaussian(link = "log"))
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    capture.output({
      m = do.call(stats::glm, pars)
    })
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }

  # names for family and link in mlr differ from glm() argument family
  parset.list[[4]]$family = "Gamma"
  parset.list[[4]]$Gamma.link = "inverse"
  parset.list[[5]]$family = "gaussian"
  parset.list[[5]]$gaussian.link = "log"

  testSimpleParsets("regr.glm", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
