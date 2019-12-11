context("regr_km")

test_that("regr_km", {
  requirePackagesOrSkip("DiceKriging", default.method = "load")

  parset.list = list(
    list(),
    # list(covtype="gauss"),
    list(covtype = "matern5_2")
  )
  dd = regr.num.df[1:50, ]
  old.predicts.list = list()
  des1 = dd[1:25, setdiff(colnames(dd), regr.num.target)]
  des2 = dd[26:50, setdiff(colnames(dd), regr.num.target)]
  y = dd[1:25, regr.num.target]
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(~1, design = des1, response = y)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(DiceKriging::km, pars)
    })
    old.predicts.list[[i]] = DiceKriging::predict(m, newdata = des2,
      type = "SK")$mean
  }
  testSimpleParsets("regr.km", dd, regr.num.target, 1:25, old.predicts.list,
    parset.list)

  ## Test that nugget.stability has an effect.
  ps = makeNumericParamSet(len = 1, lower = 0, upper = 1)
  set.seed(123)
  rs = generateRandomDesign(n = 100, ps)
  rs$y = apply(rs, 1, function(x) (x - 0.5)^2)
  tsk = makeRegrTask(data = rs, target = "y")
  lrn = makeLearner("regr.km")
  expect_error(train(lrn, tsk), "leading minor of order")
  lrn = setHyperPars(lrn, nugget.stability = 10^-8)
  m = train(lrn, tsk)
  expect_is(m$learner.model, "km")
})
