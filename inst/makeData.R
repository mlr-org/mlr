library(methods)
library(BBmisc)
library(mlr)

dn = "../data"
stopifnot(isDirectory(dn))


data(BostonHousing, package = "mlbench")
bh = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
save(bh, file = file.path(dn, "mlr.bh.RData"), compress = "xz")


data(Sonar, package = "mlbench")
sonar = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
save(sonar, file = file.path(dn, "mlr.sonar.RData"), compress = "xz")

data(wpbc, package = "mboost")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
wpbc = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
save(wpbc, file = file.path(dn, "mlr.wpbc.RData"), compress = "xz")
