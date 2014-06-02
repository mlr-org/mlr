library(methods)
library(BBmisc)
library(mlr)

dn = "../data"
stopifnot(isDirectory(dn))
file.remove(list.files(dn, full.names = TRUE))

iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
i = setdiff(1:150, 1:50 * 3)
iris.train = subsetTask(iris.task, subset = i)
iris.test = subsetTask(iris.task, subset = setdiff(1:150, i))
iris.lrn = makeLearner("classif.rpart")
save(iris.task, iris.lrn, iris.train, iris.test,
  file = file.path(dn, "mlr.iris.RData"))
