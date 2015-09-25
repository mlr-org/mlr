context("learners: general")

test_that("listLearners", {
  x1 = mylist()
  x2 = mylist("classif")
  x3 = mylist("regr")
  x4 = mylist("surv")
  x5 = mylist("cluster")
  x6 = mylist("multilabel")
  expect_true(length(x1) > 40L)
  expect_true(length(x2) > 20L)
  expect_true(length(x3) > 10L)
  expect_true(length(x4) > 1L)
  expect_true(length(x5) > 1L)
  expect_true(length(x6) > 0L)
  expect_true(setequal(x1, c(x2, x3, x4, x5, x6)))

  x6 = mylist("classif", properties = c("multiclass", "factors", "prob"))
  expect_true(length(x6) > 10 && all(x6 %in% x2))
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
  expect_true(length(x1) > 10)
  expect_true(length(x2) > 10)
  expect_true(length(x3) > 10)
  expect_true(length(intersect(x1, x3)) == 0)
  expect_true(length(intersect(x2, x3)) == 0)
  expect_true(all(x2 %in% x1))
})

