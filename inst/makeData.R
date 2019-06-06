library(mlr)
library(BBmisc)
library(devtools)


DATASEED = 7761  # nolint
COMPRESSION = "xz" # nolint

# classification
set.seed(DATASEED)
data(iris, package = "datasets")
iris.task = makeClassifTask("iris-example", data = iris, target = "Species")
use_data(iris.task, overwrite = TRUE, compress = COMPRESSION)


set.seed(DATASEED)
data(Sonar, package = "mlbench")
sonar.task = makeClassifTask("Sonar-example", data = Sonar, target = "Class")
use_data(sonar.task, overwrite = TRUE, compress = COMPRESSION)


set.seed(DATASEED)
data(BreastCancer, package = "mlbench")
BreastCancer$Id = NULL
BreastCancer = BreastCancer[complete.cases(BreastCancer), ]  # nolint
bc.task = makeClassifTask("BreastCancer-example", data = BreastCancer, target = "Class")
use_data(bc.task, overwrite = TRUE, compress = COMPRESSION)

set.seed(DATASEED)
data(PimaIndiansDiabetes, package = "mlbench")
pid.task = makeClassifTask("PimaIndiansDiabetes-example", data = PimaIndiansDiabetes, target = "diabetes", positive = "pos")
use_data(pid.task, overwrite = TRUE, compress = COMPRESSION)

set.seed(DATASEED)
data(spam, package = "kernlab")
spam.task = makeClassifTask("spam-example", data = spam, target = "type")
use_data(spam.task, overwrite = TRUE, compress = COMPRESSION)

# regression
set.seed(DATASEED)
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask("BostonHousing-example", data = BostonHousing, target = "medv")
use_data(bh.task, overwrite = TRUE, compress = COMPRESSION)

# survival analysis
set.seed(DATASEED)
data(wpbc, package = "TH.data")
wpbc$status = ifelse(wpbc$status == "R", 1L, 0L)
wpbc = wpbc[complete.cases(wpbc), ]
wpbc.task = makeSurvTask("wpbc-example", data = wpbc, target = c("time", "status"))
use_data(wpbc.task, overwrite = TRUE, compress = COMPRESSION)

set.seed(DATASEED)
data(lung, package = "survival")
lung$status = lung$status - 1
lung = lung[complete.cases(lung), ]
lung.task = makeSurvTask("lung-example", data = lung, target = c("time", "status"))
use_data(lung.task, overwrite = TRUE, compress = COMPRESSION)

# cluster analysis
set.seed(DATASEED)
data(mtcars, package = "datasets")
mtcars.task = makeClusterTask("mtcars-example", data = mtcars)
use_data(mtcars.task, overwrite = TRUE, compress = COMPRESSION)

set.seed(DATASEED)
data(agriculture, package = "cluster")
agri.task = makeClusterTask("agriculture-example", data = agriculture)
use_data(agri.task, overwrite = TRUE, compress = COMPRESSION)

# cost-sensitive classification
set.seed(DATASEED)
data(iris, package = "datasets")
cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[iris$Species, ]
iris$Species = NULL
costiris.task = makeCostSensTask("cost-sensitive iris-example", data = iris, cost = cost)
use_data(costiris.task, overwrite = TRUE, compress = COMPRESSION)

# multilabel
set.seed(DATASEED)
d = load2("thirdparty/yeast.RData")
yeast.task = makeMultilabelTask("yeast-example", data = d, target = paste0("label", 1:14))
use_data(yeast.task, overwrite = TRUE, compress = COMPRESSION)

# FDA classification
set.seed(DATASEED)
gunpoint = load2("thirdparty/gunpoint.RData")
gp.fdf = makeFunctionalData(gunpoint, fd.features = list("fd" = 2:151))
gunpoint.task = makeClassifTask(data = gp.fdf, target = "X1", positive = "1")
use_data(gunpoint.task, overwrite = TRUE, compress = COMPRESSION)

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
use_data(fuelsubset.task, overwrite = TRUE, compress = COMPRESSION)

# FDA Multiclass Classification
set.seed(DATASEED)
data(phoneme, package = "fda.usc")
ph = as.data.frame(phoneme[["learn"]]$data)
ph[, "classlearn"] = phoneme[["classlearn"]]
fdata = makeFunctionalData(ph, fd.features = list())
phoneme.task = makeClassifTask(data = fdata, target = "classlearn")
use_data(phoneme.task, overwrite = TRUE, compress = COMPRESSION)

# spatial
data(ecuador, package = "sperrorest")
coords = ecuador[, c("x", "y")]
ecuador$x = NULL
ecuador$y = NULL
spatial.task = makeClassifTask(target = "slides", data = ecuador, coordinates = coords,
                               positive = "TRUE")
use_data(spatial.task, overwrite = TRUE, compress = COMPRESSION)
