library(testthat)
print(unname(utils::installed.packages()[, 1]))
print(requireNamespace("GenSA"))
test_check("mlr", filter = "base")
