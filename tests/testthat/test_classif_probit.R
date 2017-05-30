context("classif_probit")

test_that("classif_probit", {
  m = glm(formula = binaryclass.formula, data = binaryclass.train, family = binomial(link = "probit"))

  p = predict(m, newdata = binaryclass.test, type = "response")
  p.prob = 1 - p
  p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])

  testSimple("classif.probit", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class)
  testProb("classif.probit", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.prob)

  tt = function(formula, data) {glm(formula, data = data, family = binomial(link = "probit"))}
  tp = function(model, newdata) {
    p = predict(model, newdata, type = "response")
    as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
  }

  testCV("classif.probit", binaryclass.df, binaryclass.target, tune.train = tt, tune.predict = tp)
})
