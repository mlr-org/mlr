library(Matrix)
library(MASS)
library(methods)
library(BBmisc)
library(devtools)
load_all("..")
dn = "../data"
stopifnot(isDirectory(dn))

DATASEED = 7761  # nolint

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
BreastCancer = BreastCancer[complete.cases(BreastCancer), ]  # nolint
bc.task = makeClassifTask("BreastCancer-example", data = BreastCancer, target = "Class")
save(bc.task, file = file.path(dn, "bc.task.RData"), compress = "xz")

set.seed(DATASEED)
data(PimaIndiansDiabetes, package = "mlbench")
pid.task = makeClassifTask("PimaIndiansDiabetes-example", data = PimaIndiansDiabetes, target = "diabetes", positive = "pos")
save(pid.task, file = file.path(dn, "pid.task.RData"), compress = "xz")

# one-classification (anomaly detection)
set.seed(DATASEED)
sigma = matrix(c(2, 0, 0, 5, 0, 0), 2, 2)
normal = mvrnorm(n = 1000, rep(0, 2), sigma)
colnames(normal) = paste0("V", 1:2)
normal = as.data.frame(normal)
normal$Target = "Normal"

anomaly = matrix(sample(size = 50 * 2, x = 20:100, replace = TRUE), 50, 2)
colnames(anomaly) = paste0("V", 1:2)
anomaly = as.data.frame(anomaly)
anomaly$Target = "Anomaly"
data = rbind(normal, anomaly)
data = na.omit(data)

oneclass2d.task = makeOneClassTask("one-class-2d-example", data = data, target = "Target", positive = "Anomaly", negative = "Normal")
save(oneclass2d.task, file = file.path(dn, "oneclass2d.task.RData"))

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
cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[iris$Species, ]
iris$Species = NULL
costiris.task = makeCostSensTask("cost-sensitive iris-example", data = iris, cost = cost)
save(costiris.task, file = file.path(dn, "costiris.task.RData"), compress = "xz")

# multilabel
set.seed(DATASEED)
d = load2("../thirdparty/yeast.RData")
yeast.task = makeMultilabelTask("yeast-example", data = d, target = paste0("label", 1:14))
save(yeast.task, file = file.path(dn, "yeast.task.RData"), compress = "xz")


# FDA classification
set.seed(DATASEED)
gunpoint = load2("../thirdparty/gunpoint.RData")
gp.fdf = makeFunctionalData(gunpoint, fd.features = list("fd" = 2:151))
gunpoint.task = makeClassifTask(data = gp.fdf, target = "X1", positive = "1")
save(gunpoint.task, file = file.path(dn, "gunpoint.task.RData"), compress = "xz")

# FDA regression
set.seed(DATASEED)
data(fuelSubset, package = "FDboost")
# Center / Scale Variables
fuelSubset$UVVIS = scale(fuelSubset$UVVIS, scale = FALSE)
fuelSubset$NIR = scale(fuelSubset$NIR, scale = FALSE)
fuelSubset$uvvis.lambda = with(fuelSubset, (uvvis.lambda - min(uvvis.lambda)) / (max(uvvis.lambda) - min(uvvis.lambda)))
fuelSubset$nir.lambda = with(fuelSubset, (nir.lambda - min(nir.lambda)) / (max(nir.lambda) - min(nir.lambda)))
len1 = length(fuelSubset$uvvis.lambda)
len2 = length(fuelSubset$nir.lambda)
fdf = list(UVVIS = 1:len1, NIR = (len1 + 1):(len1 + len2))
fs = data.frame("UVVIS" = fuelSubset$UVVIS, "NIR" = fuelSubset$NIR,
  "heatan" = fuelSubset$heatan,  "h20" = fuelSubset$h2o)
fs.fdf = makeFunctionalData(fs, fd.features = fdf)
fuelsubset.task = makeRegrTask(data = fs.fdf, target = "heatan")
save(fuelsubset.task, file = file.path(dn, "fuelsubset.task.RData"), compress = "xz")

# FDA Classification
gunpoint = load2("../thirdparty/gunpoint.RData")
gp.fdf = makeFunctionalData(gunpoint, fd.features = list("fd" = 2:151))
gunpoint.task = makeClassifTask(data = gp.fdf, target = "X1", positive = "1")
save(gunpoint.task, file = file.path(dn, "gunpoint.task.RData"), compress = "xz")

# FDA Multiclass Classification
set.seed(DATASEED)
data(phoneme, package = "fda.usc")
ph = as.data.frame(phoneme[["learn"]]$data)
ph[, "classlearn"] = phoneme[["classlearn"]]
fdata = makeFunctionalData(ph, fd.features = list(), exclude.cols = "label")
phoneme.task = makeClassifTask(data = fdata, target = "classlearn")
save(phoneme.task, file = file.path(dn, "phoneme.task.RData"), compress = "xz")
