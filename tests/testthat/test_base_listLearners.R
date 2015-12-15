context("listLearners")

test_that("listLearners", {
  x1 = listLearners(create = FALSE)
  expect_true(is.data.frame(x1))
  expect_true(ncol(x1) > 3L)
  expect_true(all(x1$type %in% c("classif", "regr", "cluster", "surv", "multilabel")))
  x1a = listLearners("classif", create = FALSE, properties = c("missings"))
  expect_true(length(x1a) > 10 && length(x1a) < length(x1))

  x2 = listLearners(multiclass.task, create = FALSE)
  expect_true(is.data.frame(x2))
  expect_true(nrow(x2) > 20L)
  expect_true(ncol(x2) > 3L)
  expect_true(all(x2$type == "classif"))
})
