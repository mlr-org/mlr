context("lint")
# run only on
# - PR
# - R-release
# - Travis
if (Sys.getenv("TRAVIS") == "true" && Sys.getenv("TRAVIS_PULL_REQUEST") != "false" && Sys.getenv("TRAVIS_R_VERSION_STRING") == "release") {
  test_that("lint check", {
    library("lintr")
    library("rex")
    # linters are defined in help_lint.R
    expect_lint_free(path = Sys.getenv("TRAVIS_BUILD_DIR"), linters = linters)
  })
} else {
  warning(paste("lintr test was disabled because of missing lintr.",
    "To run lintr test, please install the github version of lintr by running",
    "> devtools::install_github(\"jimhester/lintr\")", sep = "\n"))
}
