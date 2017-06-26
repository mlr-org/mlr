
library("roxygen2")

roxygenise("../../ParamHelpers")


devtools::load_all("..")
options(error = dump.frames)
configureMlr(show.info = TRUE, on.learner.error = "stop", show.learner.output = TRUE)

listCPO()

library("testthat")

devtools::test(pkg = "..")

devtools::test(pkg = "..", filter = "cpo")


makeDiscreteVectorLearnerParam("test",
  default = list("a", "b"), values = c("a", "b", "c"), len = NA)
makeDiscreteVectorLearnerParam("test",
  default = list(), values = c("a", "b", "c"), len = NA)
x
makeDiscreteVectorLearnerParam("test",
  values = c("a", "b", "c"), len = NA)

devtools::load_all("../../ParamHelpers")
devtools::test(pkg = "../../ParamHelpers")

makeLogicalVectorLearnerParam("test",
  default = c(T, T, F))


devtools::test(pkg = "..", filter = "cpo_basic")
devtools::test(pkg = "..", filter = "cpo_properties")
devtools::test(pkg = "..", filter = "cpo_datasplit")
devtools::test(pkg = "..", filter = "cpo_quick")
devtools::test(pkg = "..", filter = "cpo_cbind")
devtools::test(pkg = "..", filter = "cpo_concrete")
devtools::test(pkg = "..", filter = "cpo_meta")
devtools::test(pkg = "..", filter = "cpo_impute")
devtools::test(pkg = "..", filter = "cpo_filter")

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat"), gcFirst = FALSE)

###################################
devtools::test(pkg = "..", filter = "cpo$", reporter = c("summary", "stop"))

system.time(devtools::test(pkg = "..", filter = "cpo_dataformat", reporter = c("summary", "stop")), gcFirst = FALSE)
devtools::test(pkg = "..", filter = "ParamSetSugar")

rm(list = ls())

tst = makeCPOTargetOp("test", .type = "regr",
                      cpo.trafo = {
                        control = 0
                        data.frame(target = target + 1000)
                      }, cpo.retrafo = { data })

df = data.frame(a = c(1, 2, 1), b = c(1, , 1))

getTaskData(df %>>% tst(), target.extra = TRUE)

x = train(tst() %>>% makeLearner("regr.lm"), makeRegrTask(data = df, target = "a"))

x$learner.model$next.model$learner.model

getLearnerType(makeLearner("classif.logreg"))
