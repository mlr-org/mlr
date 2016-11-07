context("classif_earth")

test_that("classif_earth", {
  requirePackagesOrSkip("earth", default.method = "load")

  set.seed(getOption("mlr.debug.seed"))
  m = earth::earth(formula = binaryclass.formula, data = binaryclass.train, glm = list(family = binomial(link = "logit"), maxit = 50) )
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = binaryclass.test, type = "response")[,1]
  p.class = as.factor(ifelse(p > 0.5, levs[1L], levs[2L]))

  testSimple("classif.earth", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class,list(maxit = 50))
  testProb("classif.earth",   binaryclass.df, binaryclass.target, binaryclass.train.inds, p,list(maxit = 50))

})
