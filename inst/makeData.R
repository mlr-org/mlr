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
iris.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = iris.task)
save(iris.task, iris.rin, file = file.path(dn, "mlr.iris.RData"))


set.seed(DATASEED)
data(Sonar, package = "mlbench")
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
sonar.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = sonar.task)
save(sonar.task, sonar.rin, file = file.path(dn, "mlr.sonar.RData"), compress = "xz")


set.seed(DATASEED)
data(BreastCancer, package = "mlbench")
BreastCancer$Id = NULL
BreastCancer = BreastCancer[complete.cases(BreastCancer), ]
bc.task = makeClassifTask("BreastCancer-example", data = BreastCancer, target = "Class")
bc.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = bc.task)
save(bc.task, bc.rin, file = file.path(dn, "mlr.bc.RData"), compress = "xz")


# regression
set.seed(DATASEED)
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
bh.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = bh.task)
save(bh.task, bh.rin, file = file.path(dn, "mlr.bh.RData"), compress = "xz")


# survival analysis
set.seed(DATASEED)
data(wpbc, package = "TH.data")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
wpbc.task = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
wpbc.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = wpbc.task)
save(wpbc.task, wpbc.rin, file = file.path(dn, "mlr.wpbc.RData"), compress = "xz")


set.seed(DATASEED)
data(lung, package = "survival")
lung$status = lung$status - 1
lung = lung[complete.cases(lung), ]
lung.task = makeSurvTask("lung-example", data = lung, target = c("time", "status"))
lung.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = lung.task)
save(lung.task, lung.rin, file = file.path(dn, "mlr.lung.RData"), compress = "xz")


# cluster analysis
set.seed(DATASEED)
data(mtcars, package = "datasets")
mtcars.task = makeClusterTask("mtcars-example", data = mtcars)
mtcars.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = mtcars.task)
save(mtcars.task, mtcars.rin, file = file.path(dn, "mlr.mtcars.RData"), compress = "xz")


set.seed(DATASEED)
data(agriculture, package = "cluster")
agri.task = makeClusterTask("agriculture-example", data = agriculture)
agri.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = agri.task)
save(agri.task, agri.rin, file = file.path(dn, "mlr.agri.RData"), compress = "xz")


# cost-sensitive classification
set.seed(DATASEED)
data(iris, package = "datasets")
cost = matrix(runif(150*3,0,2000), 150) * (1 - diag(3))[iris$Species,]
iris$Species = NULL
costIris.task = makeCostSensTask("cost-sensitive iris-example", data = iris, cost = cost)
costIris.rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), task = costIris.task)
save(costIris.task, costIris.rin, file = file.path(dn, "mlr.costIris.RData"), compress = "xz")
