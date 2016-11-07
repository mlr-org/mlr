
context("classif_earth")

test_that("classif_earth", {
  requirePackagesOrSkip("earth", default.method = "load")

  set.seed(getOption("mlr.debug.seed"))
  m = earth::earth(formula = binaryclass.formula, data = binaryclass.train, glm = list(family = binomial(link = "logit"), maxit = 50) )
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, newdata = binaryclass.test, type = "response")[,1]
  levs = binaryclass.class.levs
  #p.prob = setColNames(cbind(1 - p, p), levs)
  p.class = as.factor(ifelse(p > 0.5, levs[1L], levs[2L]))

  testSimple("classif.earth", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class,list(maxit = 50))
  testProb("classif.earth",   binaryclass.df, binaryclass.target, binaryclass.train.inds, p,list(maxit = 50))
  
  parset.list = list(
    list(),
    list(degree = 3, nprune = 2),
    list(degree = 5, nprune = 4)
  )
  
  
  old.predicts.list = list()
  levs = binaryclass.class.levs
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset$glm = list(family = binomial(link = "logit"), maxit = 25)
    pars = list(formula = binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(earth::earth, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test, type = "response")[,1]
    old.predicts.list[[i]] = as.factor(ifelse(p > 0.5, levs[1L], levs[2L]))
  }
  
  testSimpleParsets("classif.earth", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  
})

