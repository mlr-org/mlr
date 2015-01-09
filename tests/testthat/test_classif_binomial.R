context("classif_binomial")

test_that("classif_binomial", {

  parset.list = list(
    list(link = "logit"),
    list(link = "cloglog")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    print(parset)
    set.seed(getOption("mlr.debug.seed"))
    m = glm(formula = binaryclass.formula, data = binaryclass.train, family = binomial(link = parset$link))
    p  = predict(m, newdata = binaryclass.test)
    p.prob = 1-p
    p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p.prob
  }

  testSimpleParsets("classif.binomial", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets  ("classif.binomial", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)
})





