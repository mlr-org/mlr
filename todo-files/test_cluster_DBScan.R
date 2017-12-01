context("cluster_DBScan")

test_that("cluster_DBScan", {
  skip("issue #130")

  requirePackages("RWeka")
  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = RWeka::DBScan(noclass.train, control=ctrl)
    p = predict(m, noclass.test) + 1
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.DBScan", noclass.df, character(0L), noclass.train.inds, old.predicts.list, parset.list)
})
