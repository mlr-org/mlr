context("regr_cubist")

test_that("regr_cubist", {
  requirePackagesOrSkip("Cubist", default.method = "load")

  parset.list1 = list(
    list(),
    list(committees = 2L),
    list(control = Cubist::cubistControl(extrapolation = 50L, rules = 50L))
  )
  parset.list2 = list(
    list(),
    list(committees = 2L),
    list(extrapolation = 50, rules = 50L)
  )

  old.predicts.list = list()
  X = regr.train[, setdiff(names(regr.train), regr.target)]
  y = regr.train[, regr.target]

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    parset = c(list(x = X, y = y), parset)
    m = do.call(Cubist::cubist, parset)
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.cubist", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list2)
})
