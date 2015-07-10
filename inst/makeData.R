library(methods)
library(BBmisc)
library(devtools)
load_all("..")

dn = "../data"
stopifnot(isDirectory(dn))
DATASEED = 7761

# classification
set.seed(DATASEED)
data(iris, package = "datasets")
iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
save(iris.task, file = file.path(dn, "iris.task.RData"))


set.seed(DATASEED)
data(Sonar, package = "mlbench")
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
save(sonar.task, file = file.path(dn, "sonar.task.RData"), compress = "xz")


set.seed(DATASEED)
data(BreastCancer, package = "mlbench")
BreastCancer$Id = NULL
BreastCancer = BreastCancer[complete.cases(BreastCancer), ]
bc.task = makeClassifTask("BreastCancer-example", data = BreastCancer, target = "Class")
save(bc.task, file = file.path(dn, "bc.task.RData"), compress = "xz")

set.seed(DATASEED)
data(PimaIndiansDiabetes, package = "mlbench")
pid.task = makeClassifTask("PimaIndiansDiabetes-example", data = PimaIndiansDiabetes, target = "diabetes", positive = "pos")
save(pid.task, file = file.path(dn, "pid.task.RData"), compress = "xz")

# regression
set.seed(DATASEED)
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
save(bh.task, file = file.path(dn, "bh.task.RData"), compress = "xz")


# survival analysis
set.seed(DATASEED)
data(wpbc, package = "TH.data")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
wpbc.task = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
save(wpbc.task, file = file.path(dn, "wpbc.task.RData"), compress = "xz")


set.seed(DATASEED)
data(lung, package = "survival")
lung$status = lung$status - 1
lung = lung[complete.cases(lung), ]
lung.task = makeSurvTask("lung-example", data = lung, target = c("time", "status"))
save(lung.task, file = file.path(dn, "lung.task.RData"), compress = "xz")


# cluster analysis
set.seed(DATASEED)
data(mtcars, package = "datasets")
mtcars.task = makeClusterTask("mtcars-example", data = mtcars)
save(mtcars.task, file = file.path(dn, "mtcars.task.RData"), compress = "xz")


set.seed(DATASEED)
data(agriculture, package = "cluster")
agri.task = makeClusterTask("agriculture-example", data = agriculture)
save(agri.task, file = file.path(dn, "agri.task.RData"), compress = "xz")


# cost-sensitive classification
set.seed(DATASEED)
data(iris, package = "datasets")
cost = matrix(runif(150*3,0,2000), 150) * (1 - diag(3))[iris$Species,]
iris$Species = NULL
costiris.task = makeCostSensTask("cost-sensitive iris-example", data = iris, cost = cost)
save(costiris.task, file = file.path(dn, "costiris.task.RData"), compress = "xz")

# multilabel
set.seed(DATASEED)
d = load2("../thirdparty/yeast.RData")
yeast.task = makeMultilabelTask("yeast-example", data = d, target = paste0("label", 1:14))
save(yeast.task, file = file.path(dn, "yeast.task.RData"), compress = "xz")



