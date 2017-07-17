context("classif_gam")

test_that("classif_gam", {
  requirePackagesOrSkip("mgcv", default.method = "load")
  # train("classif.gam", sonar.task)

  m = mgcv::gam(formula = getTaskFormula(binaryclass.task, explicit.features = TRUE), data = binaryclass.train, control = learnerArgsToControl(mgcv::gam.control), family = binomial)
  p = predict(m, newdata = binaryclass.test, type = "response")
  p.prob = 1-p
  p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])

  testSimple("classif.gam", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class)

})

context("classif_binomial")

test_that("classif_gam", {
  requirePackagesOrSkip("mgcv", default.method = "load")

  parset.list1 = list(
    list(family = binomial),
    list(family = binomial(link = "logit")),
    list(family = binomial(link = "cloglog")),
    list(family = binomial(link = "probit")),
    list(family = binomial(link = "cauchit")),
    list(family = binomial(link = "log"))
  )

  parset.list2 = list(
    list(),
    list(link = "logit"),
    list(link = "cloglog"),
    list(link = "probit"),
    list(link = "cauchit"),
    list(link = "log")
  )

  old.predicts.list = list()
  old.probs.list = list()
  nof = 1:55 # remove feats

  binaryclass.formula = paste0("Class ~ ", paste(sprintf("s(V%d) + ", 1:55), collapse = ''), collapse = '')
  binaryclass.formula = substr(binaryclass.formula, 1, nchar(binaryclass.formula) - 3)

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = mgcv::gam(formula = binaryclass.formula, data = binaryclass.train[, -nof], family = parset$family)
    p  = predict(m, newdata = binaryclass.test[, -nof], type = "response")
    p = 1 - p
    p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 1, 2)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p
  }

  testSimpleParsets("classif.gam", binaryclass.df[, -nof], binaryclass.target, binaryclass.train.inds,
                    old.predicts.list, parset.list2)
  testProbParsets("classif.gam", binaryclass.df[, -nof], binaryclass.target, binaryclass.train.inds,
                  old.probs.list, parset.list2)
})


test_that("regr_glm", {
  parset.list = list(
    list(),
    list(family = binomial(link = "logit")),
    list(family = binomial(link = "cloglog"))
  )

  old.predicts.list = list()

  nof = 1:55 # remove feats
  binaryclass.formula = paste0("Class ~ ", paste(sprintf("s(V%d) + ", 1:55), collapse = ''), collapse = '')
  binaryclass.formula = substr(binaryclass.formula, 1, nchar(binaryclass.formula) - 3)

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = binaryclass.formula, data = binaryclass.train[, -nof])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(mgcv::gam, pars)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = binaryclass.test[, -nof], type = "response")
    old.predicts.list[[i]] = p
  }

  # names for family and link in mlr differ from glm() argument family
  parset.list[[4]]$family = "Gamma"
  parset.list[[4]]$Gamma.link = "inverse"
  parset.list[[5]]$family = "gaussian"
  parset.list[[5]]$gaussian.link = "log"

  testSimpleParsets("regr.glm", regr.df, regr.target,
                    regr.train.inds, old.predicts.list, parset.list)
})
