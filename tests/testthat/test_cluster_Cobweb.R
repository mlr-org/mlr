context("cluster_Cobweb")

test_that("cluster_Cobweb", {
  library(RWeka)
  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(Weka_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = Cobweb(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.Cobweb", noclass.df, character(0L), noclass.train.inds,
    old.predicts.list, parset.list)
})
