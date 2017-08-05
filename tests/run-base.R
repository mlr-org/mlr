library(testthat)
print(unname(utils::installed.packages()[, 1]))
test_check("mlr", filter = "base")
