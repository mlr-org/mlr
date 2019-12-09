context("cluster_Cobweb")

test_that("cluster_Cobweb", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::Cobweb(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.Cobweb", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)
})
