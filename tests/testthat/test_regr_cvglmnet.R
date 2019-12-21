context("regr_cvglmnet")

test_that("regr_cvglmnet", {
  requirePackagesOrSkip("glmnet", default.method = "load")

  parset.list = list(
    list(),
    list(nlambda = 20L, nfolds = 5L, mxit = 95),
    list(family = "poisson", alpha = 0.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    x = regr.train
    y = x[, regr.class.col]
    x[, regr.class.col] = NULL
    info = getFixDataInfo(x, factors.to.dummies = TRUE, ordered.to.int = TRUE)
    pars = list(x = as.matrix(fixDataForLearner(x, info)), y = y)
    pars = c(pars, parset)
    glmnet::glmnet.control(factory = TRUE)
    ctrl.args = names(formals(glmnet::glmnet.control))
    set.seed(getOption("mlr.debug.seed"))
    if (any(names(pars) %in% ctrl.args)) {
      on.exit(glmnet::glmnet.control(factory = TRUE))
      do.call(glmnet::glmnet.control, pars[names(pars) %in% ctrl.args])
      m = do.call(glmnet::cv.glmnet, pars[!names(pars) %in% ctrl.args])
    } else {
      m = do.call(glmnet::cv.glmnet, pars)
    }
    newx = regr.test
    newx[, regr.class.col] = NULL
    p = as.vector(predict(m, as.matrix(fixDataForLearner(newx, info)),
      type = "response"))
    old.predicts.list[[i]] = p
  }
  testSimpleParsets("regr.cvglmnet", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
