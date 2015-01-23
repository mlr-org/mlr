context("regr_mob")

test_that("regr_mob", {
  requirePackages("party", default.method = "load")
  parset.list = list(
    list(term.feats=c("lstat", "rm"), part.feats=c("zn", "indus")),
    list(alpha=0.10, minsplit=40, term.feats=c("lstat", "rm", "crim"),
      part.feats=c("zn", "indus", "chas", "dis")),
    list(alpha=0.10, minsplit=10, trim=0.2, breakties=TRUE,
      term.feats=c("lstat", "rm", "crim", "rm", "dis"), part.feats=c("zn")),
    list(trim=0.01, bonferroni=FALSE, term.feats=c("crim"),
      part.feats=c("zn", "indus", "chas", "dis", "lstat", "rm"))
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    formula = as.formula(paste(regr.target, "~", collapse(parset$term.feats, sep=" + "),
      "|", collapse(parset$part.feats, sep=" + ")))
    parset$term.feats = parset$part.feats = NULL
    control = do.call(party::mob_control, parset)
    pars = list(formula=formula, data=regr.train, control=control)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(party::mob, pars)
    p  = predict(m, newdata=regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.mob", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)

  # FIXME: Does not work with the extenden formula for mob!
  #tt = "mob"
  #tp = function(model, newdata) predict(model, newdata)
  #
  # testCVParsets("regr.rpart", regr.df, regr.target, tune.train=tt, tune.predict=tp, parset.list=parset.list)
})
