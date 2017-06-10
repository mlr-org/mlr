
library("roxygen2")

roxygenise("..")



devtools::load_all("..")
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

debugger()

library("testthat")

devtools::test(pkg = "..", filter = "cpo$", reporter = c("summary", "stop"))
devtools::test(pkg = "..", filter = "cpo_dataformat", reporter = c("summary", "stop"))
devtools::test(pkg = "..", filter = "ParamSetSugar")


codetools::checkUsagePackage("mlr")

