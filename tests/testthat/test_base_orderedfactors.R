context("orderedfactors")

test_that("ordered factors", {
  data(BreastCancer, package = "mlbench")

  df = na.omit(BreastCancer)
  df$Id = NULL
  task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")

  expect_equal(getTaskDesc(task)$n.feat, c(numerics = 0L, factors = 4L,
    ordered = 5L, functionals = 0L))
  expect_equal(getTaskNFeats(task), 9L)

  expect_error(train("classif.lda", task), "has ordered factor")
  z = holdout("classif.rpart", task)
  expect_true(!is.na(z$aggr))
})
