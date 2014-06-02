library(methods)
library(BBmisc)
library(mlr)

dn = "../data"
stopifnot(isDirectory(dn))

data(iris, package = "datasets")
iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
train = (seq_row(iris) %% 3L) > 0L
iris.train = subsetTask(iris.task, subset = train)
iris.test = subsetTask(iris.task, subset = !train)
iris.lrn = makeLearner("classif.lda")
save(iris.task, iris.lrn, iris.train, iris.test,
  file = file.path(dn, "iris.mlr.RData"))


data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
train = (seq_row(BostonHousing) %% 3L) > 0L
bh.train = subsetTask(bh.task, subset = train)
bh.test = subsetTask(bh.task, subset = !train)
bh.lrn = makeLearner("regr.lm")
save(bh.task, bh.lrn, bh.train, bh.test,
  file = file.path(dn, "bh.mlr.RData"))


data(Sonar, package = "mlbench")
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
train = (seq_row(Sonar) %% 3L) > 0L
sonar.train = subsetTask(sonar.task, subset = train)
sonar.test = subsetTask(sonar.task, subset = !train)
sonar.lrn = makeLearner("classif.rpart")
save(sonar.task, sonar.lrn, sonar.train, sonar.test,
  file = file.path(dn, "sonar.mlr.RData"))


data(wpbc, package = "mboost")
data = wpbc
data$status = ifelse(data$status == "R", 1L, 0L)
data = data[complete.cases(data), ]
wpbc.task = makeSurvTask("wpbc-example", data = data, target = c("time", "status"))
train = (seq_row(data) %% 3L) > 0L
wpbc.train = subsetTask(wpbc.task, subset = train)
wpbc.test = subsetTask(wpbc.task, subset = !train)
wpbc.lrn = makeLearner("classif.rpart")
save(wpbc.task, wpbc.lrn, wpbc.train, wpbc.test,
  file = file.path(dn, "wpbc.mlr.RData"))
