context("regr_km")

test_that("regr_km", {
  requirePackages("DiceKriging", default.method = "load")
  parset.list = list(
    #list(covtype="gauss"),
    list(covtype="matern5_2")
  )
  dd = regr.df[1:50, c(1:3, 14)]
  old.predicts.list = list()
  des1 = dd[1:25, setdiff(colnames(dd), regr.target)]
  des2 = dd[26:50, setdiff(colnames(dd), regr.target)]
  y = dd[1:25, regr.target]
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(~1, design=des1, response=y)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(DiceKriging::km, pars)
    )
    old.predicts.list[[i]] = DiceKriging::predict(m, newdata=des2, type="SK")$mean
  }
  testSimpleParsets("regr.km", dd, regr.target, 1:25, old.predicts.list, parset.list)
})
