context("regr_glmnet")

test_that("regr_glmnet", {
  requirePackagesOrSkip("glmnet", default.method = "load")

  parset.list = list(
    list(),
    list(alpha = 0.7, fdev = 0.0001),
    list(s = 0.3)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    s = parset[["s"]]
    if (is.null(s)) s = 0.01
    parset[["s"]] = NULL
    ind = match(regr.target, names(regr.train))
    x = regr.train[, -ind]
    x$chas = as.numeric(x$chas)
    y = regr.train[, ind]
    pars = list(x = as.matrix(x), y = y, family = "gaussian")
    pars = c(pars, parset)
    glmnet::glmnet.control(factory = TRUE)
    ctrl.args = names(formals(glmnet::glmnet.control))
    if (any(names(pars) %in% ctrl.args)) {
      on.exit(glmnet::glmnet.control(factory = TRUE))
      do.call(glmnet::glmnet.control, pars[names(pars) %in% ctrl.args])
      m = do.call(glmnet::glmnet, pars[!names(pars) %in% ctrl.args])
    } else {
      m = do.call(glmnet::glmnet, pars)
    }
    newx = regr.test[, -ind]
    newx$chas = as.numeric(newx$chas)
    old.predicts.list[[i]] = predict(m, as.matrix(newx), s = s)[, 1]
  }
  test.dat = regr.df
  test.dat$chas = as.numeric(test.dat$chas)
  testSimpleParsets("regr.glmnet", test.dat, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})


test_that("regr_glmnet works with poisson", {
  requirePackagesOrSkip("glmnet", default.method = "load")
  # set some dummy counts
  d = regr.df
  d[, regr.target] = sample(1:100, getTaskSize(regr.task), replace = TRUE)
  task = makeRegrTask(data = d, target = regr.target)
  lrn = makeLearner("regr.glmnet", family = "poisson")
  r = holdout(lrn, task)
  expect_true(!is.na(r$aggr))
})
