context("listLearners")

test_that("listLearners", {
  x1 = listLearners(create = FALSE, table = FALSE)
  expect_true(is.character(x1))
  expect_true(length(x1) > 100)
  expect_true(!any(is.na(x1)))
  x1a = listLearners("classif", create = FALSE, table = FALSE, properties = c("missings"))

  x2 = listLearners(create = FALSE, table = TRUE)
  expect_true(is.data.frame(x2))
  expect_equal(length(x1), nrow(x2))
  expect_true(ncol(x2) > 3L)
  expect_true(all(x2$type %in% c("classif", "regr", "cluster", "surv", "multilabel")))
  x2a = listLearners("classif", create = FALSE, table = TRUE, properties = c("missings"))
  expect_equal(length(x1a), nrow(x2a))

  x3 = listLearners(multiclass.task, create = FALSE, table = TRUE)
  expect_true(is.data.frame(x3))
  expect_true(nrow(x3) > 20L)
  expect_true(ncol(x3) > 3L)
  expect_true(all(x3$type == "classif"))
})
