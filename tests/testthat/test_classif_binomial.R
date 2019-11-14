context("classif_binomial")

test_that("classif_binomial", {
  parset.list1 = list(
    list(family = binomial),
    list(family = binomial(link = "logit")),
    list(family = binomial(link = "cloglog"))
  )

  parset.list2 = list(
    list(),
    list(link = "logit"),
    list(link = "cloglog")
  )

  old.predicts.list = list()
  old.probs.list = list()
  nof = 1:55 # remove feats

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = glm(formula = binaryclass.formula, data = binaryclass.train[, -nof], family = parset$family)
    p = predict(m, newdata = binaryclass.test[, -nof], type = "response")
    p = 1 - p
    p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 1, 2)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p
  }

  testSimpleParsets("classif.binomial", binaryclass.df[, -nof],
    binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list2)
  testProbParsets("classif.binomial", binaryclass.df[, -nof],
    binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list2)
})
