context("learners_all_general")

test_that("learners_all_general listLearners", {
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
  expect_true(setequal(x1$id, c(x2$id, x3$id, x4$id, x5$id, x6$id)))

  x6 = mylist("classif", properties = c("multiclass", "factors", "prob"))
  expect_true(nrow(x6) > 10 && all(x6$id %in% x2$id))
})

test_that("learners_all_general listLearners doesn't load packages", {
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
  expect_true(length(intersect(x1$id, x3$id)) == 0)
  expect_true(length(intersect(x2$id, x3$id)) == 0)
  expect_true(all(x2$id %in% x1$id))
})

test_that("fuzzy matching works for mistyped learners", {
  expect_error(makeLearner("classi.randomFore", config = list(on.par.without.desc = "quiet"),
    expected = "classif.randomForest"))
})
