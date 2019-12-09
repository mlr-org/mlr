context("cluster_FarthestFirst")

test_that("cluster_FarthestFirst", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(N = 3)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::FarthestFirst(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.FarthestFirst", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)
})
