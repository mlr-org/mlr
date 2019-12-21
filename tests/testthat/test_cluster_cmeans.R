context("cluster_cmeans")

test_that("cluster_cmeans", {
  requirePackagesOrSkip("e1071", default.method = "load")

  centers = matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 4)
  parset.list = list(
    list(),
    list(centers = centers),
    list(reltol = 0.01)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    m = e1071::cmeans(noclass.train, centers = centers)
    p = as.integer(clue::cl_predict(m, newdata = noclass.test,
      type = "class_ids"))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.cmeans", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)

  # test fuzzy clustering memberships
  m = e1071::cmeans(noclass.train, centers = centers)
  p = clue::cl_predict(m, newdata = noclass.test, type = "memberships")
  class(p) = "matrix"

  lrn = makeLearner("cluster.cmeans", predict.type = "prob", centers = centers)
  m = train(lrn, task = makeClusterTask(data = noclass.train))
  pp = as.matrix(predict(m, newdata = noclass.test)$data)

  expect_equal(getMaxIndexOfRows(p), pp[, 1], check.attributes = FALSE)
  expect_equal(p[, 1], pp[, 2], check.attributes = FALSE)
  expect_equal(p[, 2], pp[, 3], check.attributes = FALSE)
})
