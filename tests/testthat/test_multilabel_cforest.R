context("multilabel_cforest")

test_that("multilabel_cforest", {
  requirePackagesOrSkip("party", default.method = "load")
  
  parset.list = list(
    list(),
    list(control = party::cforest_unbiased(mtry = 2)),
    list(control = party::cforest_unbiased(ntree = 200))
  )
  parset.list2 = list(
    list(),
    list(mtry = 2),
    list(ntree = 200)
  )
  
  old.predicts.list = list()
  old.probs.list = list()
  
  multilabel.formula = as.formula(paste(paste(multilabel.target, collapse = "+"), "~."))
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(multilabel.formula, data = multilabel.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(party::cforest, pars)
    p = predict(m, newdata = multilabel.test, type = "prob")
    p2 = do.call(rbind, p)
    p2 = t(apply(p2, 1L, function(x) {ifelse(x == max(x), TRUE, FALSE)}))
    old.predicts.list[[i]] = data.frame(p2)
    old.probs.list[[i]] = data.frame(p)
  }
  
  testProbParsets("multilabel.cforest", multilabel.df, multilabel.target,
    multilabel.train.inds, old.probs.list, parset.list2)
  
})