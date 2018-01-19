context("regr_logicreg")

skip("still in todo")

#test_that("regr_logicreg", {
#  requirePackages("LogicReg")
#  set.seed(1)
#  mydata = as.data.frame(matrix(rbinom(100*5, 1, 0.5), 100, 5))
#  mydata$y = rnorm(100)
#  rt = makeRegrTask(target="y", data=mydata)
#
#  parset.list1 = list(
#    list(seed=debug.seed, type=2),
#    list(seed=debug.seed, type=2, ntrees=1, tree.control=LogicReg::logreg.tree.control(treesize=3, minmass=5))
#  )
#  parset.list2 = list(
#    list(seed=debug.seed),
#    list(seed=debug.seed, ntrees=1L, treesize=3L, minmass=5L)
#  )
#
#  old.predicts.list = list()
#
#  for (i in seq_along(parset.list1)) {
#    parset = parset.list1[[i]]
#    pars = list(resp=mydata$y[1:60], bin=mydata[1:60, 1:5], select=1L)
#    pars = c(pars, parset)
#    set.seed(getOption("mlr.debug.seed"))
#    m = do.call(LogicReg::logreg, pars)
#    set.seed(getOption("mlr.debug.seed"))
#    p = predict(m, newbin=mydata[61:100, 1:5])
#    old.predicts.list[[i]] = p
#  }
#
#  testSimpleParsets("regr.logicreg", mydata, "y", 1:60, old.predicts.list, parset.list2)
#})
