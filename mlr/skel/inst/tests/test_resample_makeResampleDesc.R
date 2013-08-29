context("makeResampleDesc")

test_that("makeResampleDesc", {
  desc1 = makeResampleDesc("CV", predict="test", iters=2)
  expect_equal(desc1$iters, 2)
  expect_equal(desc1$predict, "test")
  expect_error(makeResampleDesc("Foo", predict="test", iters=2), "Argument method must be any of")
  expect_error(makeResampleDesc("CV", predict="Foo", iters=2), "Argument predict must be any of")
})