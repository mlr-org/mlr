context("classif_binomial")

test_that("classif_binomial", {

  parset.list = list(
    list(link = "logit"),
    list(link = "cloglog")
  )

  old.predicts.list = list()
  old.probs.list = list()
  nof = 1:55 # remove feats

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = glm(formula = binaryclass.formula, data = binaryclass.train[, -nof], family = binomial(link = parset$link))
    p  = predict(m, newdata = binaryclass.test[,-nof], type = "response")
    p = 1 - p
    p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 1, 2)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p
  }

  testSimpleParsets("classif.binomial", binaryclass.df[,-nof], binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets  ("classif.binomial", binaryclass.df[,-nof], binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)
})





