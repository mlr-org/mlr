context("classif_sparseMDA")

test_that("classif_sparseMDA", {
  requirePackages(c("sparseLDA", "MASS", "elasticnet"))
  
  parset.list = list(
    list(),
    list(lambda = 1, maxIte = 50),
    list(lambda = 2, maxIte = 60),
    list(tol = 1e-4, maxIte = 100),
    list(tol = 1e-5, lambda = 0.5),
    list(tol = 3e-5, lambda = 0.8, maxIte = 150)
  )
  
  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    target.col = which(colnames(multiclass.train) == multiclass.target)
    X = multiclass.train[, -target.col]
    y = multiclass.train[, multiclass.target]
    lvls = unique(as.character(y))
    y = sapply(lvls, function(lvl) as.integer(as.character(y) == lvl))
    pars = c(list(x = X, y = y), parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(sparseLDA::smda, pars)
    old.predicts.list[[i]] = sparseLDA:::predict.smda(m, 
      newdata = subset(multiclass.test, select = m$varNames))$class
    old.probs.list[[i]] = sparseLDA:::predict.smda(m, 
      newdata = subset(multiclass.test, select = m$varNames))$posterior
  }
  
  testSimpleParsets("classif.sparseMDA", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets  ("classif.sparseMDA", multiclass.df, multiclass.target, 
    multiclass.train.inds, old.probs.list, parset.list)
})

