context("learners_all_general")

test_that("listLearners", {
  x1 = mylist()
  x2 = mylist("classif")
  x3 = mylist("regr")
  x4 = mylist("surv")
  x5 = mylist("cluster")
  x6 = mylist("multilabel")
  expect_true(nrow(x1) > 40L)
  expect_true(nrow(x2) > 20L)
  expect_true(nrow(x3) > 10L)
  expect_true(nrow(x4) > 1L)
  expect_true(nrow(x5) > 1L)
  expect_true(nrow(x6) > 0L)
  expect_true(setequal(x1$class, c(x2$class, x3$class, x4$class, x5$class, x6$class)))

  x6 = mylist("classif", properties = c("multiclass", "factors", "prob"))
  expect_true(nrow(x6) > 10 && all(x6$class %in% x2$class))
})

test_that("listLearners doesn't load packages", {
  npacks.before = length(search())
  mylist("classif")
  npacks.after = length(search())

  expect_equal(npacks.before, npacks.after)
})

test_that("listLearners for task", {
  x1 = mylist(binaryclass.task)
  x2 = mylist(multiclass.task)
  x3 = mylist(regr.task)
  expect_true(nrow(x1) > 10)
  expect_true(nrow(x2) > 10)
  expect_true(nrow(x3) > 10)
  expect_true(length(intersect(x1$class, x3$class)) == 0)
  expect_true(length(intersect(x2$class, x3$class)) == 0)
  expect_true(all(x2$class %in% x1$class))
})

