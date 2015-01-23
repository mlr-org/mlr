context("cluster_SimpleKMeans")

test_that("cluster_SimpleKMeans", {
  requirePackages("RWeka", default.method = "load")
  parset.list = list(
    list(),
    list(N = 5L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    ctrl = do.call(RWeka::Weka_control, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = RWeka::SimpleKMeans(noclass.train, control = ctrl)
    p = predict(m, noclass.test) + 1L
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.SimpleKMeans", noclass.df, character(0L), noclass.train.inds,
    old.predicts.list, parset.list)
})
