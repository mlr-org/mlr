context("train")

test_that("train works with subset", {
  subs = 1:5
  mod = train("classif.rpart", binaryclass.task, subset = subs)
  expect_equal(mod$subset, subs)
  expect_equal(length(mod$learner.model$y), length(subs))

  subs = c(TRUE, FALSE, TRUE, TRUE)
  mod = train("classif.rpart", binaryclass.task, subset = subs)
  expect_equal(mod$subset, c(1L, 3L, 4L))
  expect_equal(length(mod$learner.model$y), sum(subs))
})
