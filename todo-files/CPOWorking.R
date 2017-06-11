
library("roxygen2")

roxygenise("..")



devtools::load_all("..")
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)



library("testthat")

devtools::test(pkg = "..", filter = "cpo$")

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat"), gcFirst = FALSE)


devtools::test(pkg = "..", filter = "cpo$", reporter = c("summary", "stop"))

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat", reporter = c("summary", "stop")), gcFirst = FALSE)


devtools::test(pkg = "..", filter = "ParamSetSugar")

debugger()

1 + 1
2 + 2
3 + 3

codetools::checkUsagePackage("mlr")

rm(list = ls())
