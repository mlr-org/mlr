context("cluster_XMeans")

test_that("cluster_XMeans", {
  requirePackagesOrSkip("RWeka", default.method = "load")
  # RWeka::WPM("refresh-cache")
  # RWeka::WPM("install-package", "XMeans")

  parset.list = list(
    list(),
    list(I = 1)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    m = RWeka::XMeans(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.XMeans", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)
})
