context("learnerArgsToControl")

test_that("learnerArgsToControl with list returns the input", {

  checkLearnerArgsToControl = function(arg1, arg2, ...) {
    learnerArgsToControl(control = list, arg1, arg2, ...)
  }

  arg1 = 1
  arg2 = "foo"
  test1 = checkLearnerArgsToControl(arg1, arg2)
  expect_equal(list(arg1 = arg1, arg2 = arg2), test1)

  # test missing values
  arg1 = quote(expr = )  # nolint
  arg2 = quote(expr = )  # nolint
  test2 = checkLearnerArgsToControl(arg1, arg2)
  expect_equal(list(), test2)

  # test for dots
  test3 = checkLearnerArgsToControl(arg1, arg2, arg3 = 1000)
  expect_equal(list(arg3 = 1000), test3)

})


test_that("learnerArgsToControl works with a control object", {

  checkLearnerArgsToControlWithControl = function(fdev, devmax, ...) {
    learnerArgsToControl(control = glmnet::glmnet.control, fdev, devmax, ...)
  }

  on.exit(glmnet::glmnet.control(factory = TRUE))
  fdev = 0.0033
  devmax = 0.888

  glmnet::glmnet.control(factory = TRUE) # set control params to default
  test1 = checkLearnerArgsToControlWithControl(fdev, devmax)
  expect_equal(test1, glmnet::glmnet.control(fdev = fdev, devmax = devmax))

  devmax = quote(expr = )  # nolint
  glmnet::glmnet.control(factory = TRUE)
  test2 = checkLearnerArgsToControlWithControl(fdev, devmax, mnlam = 3)
  expect_equal(test2, glmnet::glmnet.control(fdev = fdev, devmax = devmax, mnlam = 3))
})

