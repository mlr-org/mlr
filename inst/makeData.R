library(methods)
library(BBmisc)
library(mlr)

dn = "../data"
stopifnot(isDirectory(dn))
DATASEED = 7761

set.seed(DATASEED)
data(iris, package = "datasets")
iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
iris.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = iris.task)
save(iris.task, iris.rin, file = file.path(dn, "mlr.iris.RData"))

set.seed(DATASEED)
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
bh.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = bh.task)
save(bh.task, bh.rin, file = file.path(dn, "mlr.bh.RData"), compress = "xz")


set.seed(DATASEED)
data(Sonar, package = "mlbench")
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
sonar.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = sonar.task)
save(sonar.task, sonar.rin, file = file.path(dn, "mlr.sonar.RData"), compress = "xz")

set.seed(DATASEED)
data(wpbc, package = "TH.data")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
wpbc.task = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
wpbc.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = wpbc.task)
save(wpbc.task, wpbc.rin, file = file.path(dn, "mlr.wpbc.RData"), compress = "xz")
