library(methods)
library(BBmisc)
library(mlr)

dn = "../data"
stopifnot(isDirectory(dn))

data(iris, package = "datasets")
train = (seq_row(iris) %% 3L) > 0
iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
iris.lrn = makeLearner("classif.lda")
iris.train = subsetTask(iris.task, subset = train)
iris.test = subsetTask(iris.task, subset = !train)
save(iris.task, iris.lrn, iris.train, iris.test, file = file.path(dn, "mlr.iris.RData"))

data(BostonHousing, package = "mlbench")
train = (seq_row(BostonHousing) %% 3L) > 0L
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
bh.lrn = makeLearner("regr.lm")
bh.train = subsetTask(bh.task, subset = train)
bh.test = subsetTask(bh.task, subset = !train)
save(bh.task, bh.lrn, bh.train, bh.test, file = file.path(dn, "mlr.bh.RData"), compress = "xz")


data(Sonar, package = "mlbench")
train = (seq_row(Sonar) %% 3L) > 0L
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
sonar.lrn = makeLearner("classif.rpart")
sonar.train = subsetTask(sonar.task, subset = train)
sonar.test = subsetTask(sonar.task, subset = !train)
save(sonar.task, sonar.lrn, sonar.train, sonar.test, file = file.path(dn, "mlr.sonar.RData"), compress = "xz")

data(wpbc, package = "mboost")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
train = (seq_row(wpbc) %% 3L) > 0L
wpbc.task = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
wpbc.lrn = makeLearner("surv.coxph")
wpbc.train = subsetTask(wpbc.task, subset = train)
wpbc.test = subsetTask(wpbc.task, subset = !train)
save(wpbc.task, wpbc.lrn, wpbc.train, wpbc.test, file = file.path(dn, "mlr.wpbc.RData"), compress = "xz")
