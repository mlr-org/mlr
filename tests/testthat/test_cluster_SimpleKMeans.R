context("cluster_SimpleKMeans")

test_that("cluster_SimpleKMeans", {
  requirePackagesOrSkip("RWeka", default.method = "load")

  parset.list = list(
    list(),
    list(N = 5L)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::SimpleKMeans(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1L
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.SimpleKMeans", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)
})
