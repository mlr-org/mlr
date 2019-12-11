context("regr_mob")

test_that("regr_mob", {
  requirePackagesOrSkip("party", default.method = "load")

  parset.list = list(
    list(),
    list(term.feats = c("lstat", "rm"), part.feats = c("zn", "indus")),
    list(alpha = 0.10, minsplit = 40, term.feats = c("lstat", "rm", "crim"),
      part.feats = c("zn", "indus", "chas", "dis")),
    list(alpha = 0.10, minsplit = 10, trim = 0.2, breakties = TRUE,
      term.feats = c("lstat", "rm", "crim", "rm", "dis"), part.feats = "zn"),
    list(trim = 0.01, bonferroni = FALSE, term.feats = "crim",
      part.feats = c("zn", "indus", "chas", "dis", "lstat", "rm"))
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    feats = getTaskFeatureNames(regr.task)
    if (is.null(parset$part.feats)) {
      part.feats = feats
    } else {
      part.feats = parset$part.feats
    }
    if (is.null(parset$term.feats)) {
      term.feats = feats
    } else {
      term.feats = parset$term.feats
    }
    formula = as.formula(paste(regr.target, "~", collapse(term.feats,
      sep = " + "), "|", collapse(part.feats, sep = " + ")))
    parset$term.feats = parset$part.feats = NULL
    control = do.call(party::mob_control, parset)
    pars = list(formula = formula, data = regr.train, control = control)
    m = do.call(party::mob, pars)
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.mob", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)

  # FIXME: Does not work with the extenden formula for mob!
  # tt = "mob"
  # tp = function(model, newdata) predict(model, newdata)
  #
  # testCVParsets("regr.rpart", regr.df, regr.target, tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
