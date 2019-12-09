data(Sonar, package = "mlbench", envir = environment())
data(BreastCancer, package = "mlbench", envir = environment())
data(spatial.task, package = "mlr", envir = environment())

binaryclass.df = Sonar
binaryclass.formula = Class ~ .
binaryclass.target = "Class"
binaryclass.train.inds = c(1:50, 100:150)
binaryclass.test.inds = setdiff(seq_len(nrow(binaryclass.df)), binaryclass.train.inds)
binaryclass.train = binaryclass.df[binaryclass.train.inds, ]
binaryclass.test = binaryclass.df[binaryclass.test.inds, ]
binaryclass.class.col = 61
binaryclass.class.levs = levels(binaryclass.df[, binaryclass.class.col])
binaryclass.task = makeClassifTask("binary", data = binaryclass.df, target = binaryclass.target)

binaryclass.spatial.df = spatial.task$env$data
coordinates = spatial.task$coordinates
binaryclass.spatial.formula = slides ~ .
binaryclass.spatial.target = "slides"
binaryclass.spatial.train.inds = c(1:300, 600:900)
binaryclass.spatial.test.inds = setdiff(seq_len(nrow(binaryclass.spatial.df)), binaryclass.spatial.train.inds)
binaryclass.spatial.train = binaryclass.spatial.df[binaryclass.spatial.train.inds, ]
binaryclass.spatial.test = binaryclass.spatial.df[binaryclass.spatial.test.inds, ]
binaryclass.spatial.class.col = 3
binaryclass.spatial.class.levs = levels(binaryclass.spatial.df[, binaryclass.spatial.class.col])
binaryclass.spatial.task = makeClassifTask("binary", data = binaryclass.spatial.df, target = binaryclass.spatial.target, coordinates = coordinates)

multiclass.df = iris
multiclass.formula = Species ~ .
multiclass.target = "Species"
multiclass.train.inds = c(1:30, 51:80, 101:130)
multiclass.test.inds = setdiff(1:150, multiclass.train.inds)
multiclass.train = multiclass.df[multiclass.train.inds, ]
multiclass.test = multiclass.df[multiclass.test.inds, ]
multiclass.class.col = 5
multiclass.task = makeClassifTask("multiclass", data = multiclass.df, target = multiclass.target)

multiclass.small.df = iris[c(1:3, 51:53, 101:103), ]
multiclass.small.formula = Species ~ .
multiclass.small.target = "Species"
multiclass.small.train.inds = c(1:2, 4:5, 7:8)
multiclass.small.test.inds = setdiff(1:9, multiclass.small.train.inds)
multiclass.small.train = multiclass.small.df[multiclass.small.train.inds, ]
multiclass.small.test = multiclass.small.df[multiclass.small.test.inds, ]
multiclass.small.class.col = 5
multiclass.small.task = makeClassifTask("multiclass", data = multiclass.small.df, target = multiclass.small.target)

multilabel.df = iris
multilabel.df[, "y1"] = rep(c(TRUE, FALSE), 75L)
multilabel.df[, "y2"] = rep(c(FALSE, TRUE), 75L)
multilabel.target = c("y1", "y2")
multilabel.train.inds = c(1:30, 51:80, 101:130)
multilabel.test.inds = setdiff(1:150, multilabel.train.inds)
multilabel.train = multilabel.df[multilabel.train.inds, ]
multilabel.test = multilabel.df[multilabel.test.inds, ]
multilabel.task = makeMultilabelTask("multilabel", data = multilabel.df, target = multilabel.target)
multilabel.formula.cbind = as.formula(paste("cbind(", paste(multilabel.target, collapse = ",", sep = " "), ")  ~ .", sep = ""))
multilabel.formula = as.formula(paste(paste(multilabel.target, collapse = "+"), "~."))
multilabel.small.inds = c(1, 52, 53, 123)

noclass.df = iris[, -5]
noclass.train.inds = c(1:30, 51:80, 101:130)
noclass.test.inds = setdiff(1:150, noclass.train.inds)
noclass.train = noclass.df[noclass.train.inds, ]
noclass.test = noclass.df[noclass.test.inds, ]
noclass.task = makeClusterTask("noclass", data = noclass.df)

data(BostonHousing, package = "mlbench", envir = environment())
regr.df = BostonHousing
regr.formula = medv ~ .
regr.target = "medv"
regr.train.inds = seq(1, 506, 7)
regr.test.inds = setdiff(seq_len(nrow(regr.df)), regr.train.inds)
regr.train = regr.df[regr.train.inds, ]
regr.test = regr.df[regr.test.inds, ]
regr.class.col = 14
regr.task = makeRegrTask("regrtask", data = regr.df, target = regr.target)

regr.small.df = BostonHousing[150:160, ]
regr.small.formula = medv ~ .
regr.small.target = "medv"
regr.small.train.inds = 1:7
regr.small.test.inds = setdiff(seq_len(nrow(regr.small.df)), regr.small.train.inds)
regr.small.train = regr.small.df[regr.small.train.inds, ]
regr.small.test = regr.small.df[regr.small.test.inds, ]
regr.small.class.col = 14
regr.small.task = makeRegrTask("regrtask", data = regr.small.df, target = regr.small.target)

regr.num.df = regr.df[, sapply(regr.df, is.numeric)]
regr.num.formula = regr.formula
regr.num.target = regr.target
regr.num.train.inds = regr.train.inds
regr.num.test.inds = regr.test.inds
regr.num.train = regr.num.df[regr.num.train.inds, ]
regr.num.test = regr.num.df[regr.num.test.inds, ]
regr.num.class.col = 13
regr.num.task = makeRegrTask("regrnumtask", data = regr.num.df, target = regr.num.target)

regr.na.num.df = regr.num.df[1:10, ]
regr.na.num.df[1, 1] = NA
regr.na.num.formula = regr.num.formula
regr.na.num.target = regr.num.target
regr.na.num.train.inds = regr.num.train.inds
regr.na.num.test.inds = regr.num.test.inds
regr.na.num.train = regr.na.num.df[regr.na.num.train.inds, ]
regr.na.num.test = regr.na.num.df[regr.na.num.test.inds, ]
regr.na.num.class.col = 13
regr.na.num.task = makeRegrTask("regrnanumdf", data = regr.na.num.df, target = regr.na.num.target)

getSurvData = function(n = 100, p = 10) {

  set.seed(1)
  beta = c(rep(1, 10), rep(0, p - 10))
  x = matrix(rnorm(n * p), n, p)
  colnames(x) = sprintf("x%01i", 1:p)
  real.time = -(log(runif(n))) / (10 * exp(drop(x %*% beta)))
  cens.time = rexp(n, rate = 1 / 10)
  status = ifelse(real.time <= cens.time, TRUE, FALSE)
  obs.time = ifelse(real.time <= cens.time, real.time, cens.time) + 1

  # mark large outliers in survival as censored
  q = quantile(obs.time, .90)
  i = which(obs.time > q)
  obs.time[i] = q
  cens.time[i] = FALSE

  return(cbind(data.frame(time = obs.time, status = status), x))
}
surv.df = getSurvData()
surv.formula = survival::Surv(time, status) ~ .
surv.target = c("time", "status")
surv.train.inds = seq(1, floor(2 / 3 * nrow(surv.df)))
surv.test.inds = setdiff(seq_len(nrow(surv.df)), surv.train.inds)
surv.train = surv.df[surv.train.inds, ]
surv.test = surv.df[surv.test.inds, ]
surv.task = makeSurvTask("survtask", data = surv.df, target = surv.target)
rm(getSurvData)

data("gunpoint.task", package = "mlr")
data("fuelsubset.task", package = "mlr")
fda.binary.gp.task = gunpoint.task
suppressMessages({
  gp = getTaskData(gunpoint.task, subset = seq_len(100), functionals.as = "dfcols")
})
gp.fdf = makeFunctionalData(gp[, seq_len(31)], fd.features = list("fd" = 2:31))
fda.binary.gp.task.small = makeClassifTask(data = gp.fdf, target = "X1")
fda.regr.fs.task = fuelsubset.task

# nonsense fda multiclass task
fda.multiclass.df = iris
fda.multiclass.formula = Species ~ .
fda.multiclass.target = "Species"
fda.multiclass.train.inds = c(1:30, 51:80, 101:130)
fda.multiclass.test.inds = setdiff(1:150, multiclass.train.inds)
fda.multiclass.train = multiclass.df[multiclass.train.inds, ]
fda.multiclass.test = multiclass.df[multiclass.test.inds, ]
fda.multiclass.class.col = 5
mc.fdf = makeFunctionalData(fda.multiclass.df, fd.features = list("fd1" = 1:2, "fd2" = 3:4))
fda.multiclass.task = makeClassifTask("multiclass", data = mc.fdf, target = multiclass.target)

costsens.feat = iris
costsens.costs = matrix(runif(150L * 3L, min = 0, max = 1), 150L, 3L)
costsens.task = makeCostSensTask("costsens", data = costsens.feat, costs = costsens.costs)

ns.svg = c(svg = "http://www.w3.org/2000/svg")
black.circle.xpath = "/svg:svg//svg:circle[contains(@style, 'fill: #000000')]"
grey.rect.xpath = "/svg:svg//svg:rect[contains(@style, 'fill: #EBEBEB;')]"
red.circle.xpath = "/svg:svg//svg:circle[contains(@style, 'fill: #F8766D')]"
blue.circle.xpath = "/svg:svg//svg:circle[contains(@style, 'fill: #619CFF')]"
green.circle.xpath = "/svg:svg//svg:circle[contains(@style, 'fill: #00BA38')]"
black.line.xpath = "/svg:svg//svg:polyline[not(contains(@style, 'stroke:'))]"
black.line.xpath2 = "/svg:svg//svg:polyline[contains(@style, 'stroke: #000000')]"
blue.line.xpath = "/svg:svg//svg:polyline[contains(@style, 'stroke: #00BFC4;')]"
mediumblue.line.xpath = "/svg:svg//svg:polyline[contains(@style, 'stroke: #3366FF;')]"
red.line.xpath = "/svg:svg//svg:polyline[contains(@style, 'stroke: #F8766D;')]"
red.rug.line.xpath = "/svg:svg//svg:line[contains(@style, 'stroke: #FF0000;')]"
black.bar.xpath = "/svg:svg//svg:rect[contains(@style, 'fill: #595959;')]"

test.confMatrix = function(p) {

  lvls = getTaskClassLevels(p$task.desc)
  l = length(lvls)

  # test absolute
  cm = calculateConfusionMatrix(p, relative = FALSE)
  expect_true(is.matrix(cm$result) && nrow(cm$result) == l + 1 && ncol(cm$result) == l + 1)
  expect_set_equal(cm$result[1:l, l + 1], cm$result[l + 1, 1:l])

  # test absolute number of errors
  d = cm$result[1:l, 1:l]
  diag(d) = 0
  expect_true(sum(unlist(d)) == cm$result[l + 1, l + 1])

  # test absolute with sums
  cm = calculateConfusionMatrix(p, sums = TRUE)
  expect_true(is.matrix(cm$result) && nrow(cm$result) == l + 2 && ncol(cm$result) == l + 2)
  expect_set_equal(cm$result[1:l, l + 1], cm$result[l + 1, 1:l])
  # test absolute number of errors
  d = cm$result[1:l, 1:l]
  diag(d) = 0
  expect_true(sum(unlist(d)) == cm$result[l + 1, l + 1])

  # test relative
  cm = calculateConfusionMatrix(p, relative = TRUE)

  # sums have to be 1 or 0 (if no observation in that group)
  expect_true(all(rowSums(cm$relative.row[, 1:l]) == 1 |
    rowSums(cm$relative.row[, 1:l]) == 0))
  expect_true(all(colSums(cm$relative.col[1:l, ]) == 1 |
    colSums(cm$relative.col[1:l, ]) == 0))
}
# used in `test_base_rankBaseFilters.R` ----------------------------------------

task.filters.rank = structure(list(type = "surv", weights = NULL,
  blocking = NULL, coordinates = NULL, task.desc = structure(list(
    id = "VET", type = "surv", target = c("time", "status"
    ), size = 137L, n.feat = c(numerics = 9L, factors = 0L,
      ordered = 0L, functionals = 0L), has.missings = FALSE,
    has.weights = FALSE, has.blocking = FALSE, has.coordinates = FALSE), class = c("SurvTaskDesc",
    "SupervisedTaskDesc", "TaskDesc"))), class = c("SurvTask",
  "SupervisedTask", "Task"))
task.filters.rank$env$data = structure(list(trt = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), time = c(72,
  411, 228, 126, 118, 10, 82, 110, 314, 100, 42, 8, 144, 25, 11,
  30, 384, 4, 54, 13, 123, 97, 153, 59, 117, 16, 151, 22, 56, 21,
  18, 139, 20, 31, 52, 287, 18, 51, 122, 27, 54, 7, 63, 392, 10,
  8, 92, 35, 117, 132, 12, 162, 3, 95, 177, 162, 216, 553, 278,
  12, 260, 200, 156, 182, 143, 105, 103, 250, 100, 999, 112, 87,
  231, 242, 991, 111, 1, 587, 389, 33, 25, 357, 467, 201, 1, 30,
  44, 283, 15, 25, 103, 21, 13, 87, 2, 20, 7, 24, 99, 8, 99, 61,
  25, 95, 80, 51, 29, 24, 18, 83, 31, 51, 90, 52, 73, 8, 36, 48,
  7, 140, 186, 84, 19, 45, 80, 52, 164, 19, 53, 15, 43, 340, 133,
  111, 231, 378, 49), status = c(TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE), karno = c(60, 70, 60, 60, 70,
  20, 40, 80, 50, 70, 60, 40, 30, 80, 70, 60, 60, 40, 80, 60, 40,
  60, 60, 30, 80, 30, 50, 60, 80, 40, 20, 80, 30, 75, 70, 60, 30,
  60, 80, 60, 70, 50, 50, 40, 40, 20, 70, 40, 80, 80, 50, 80, 30,
  80, 50, 80, 50, 70, 60, 40, 80, 80, 70, 90, 90, 80, 80, 70, 60,
  90, 80, 80, 50, 50, 70, 70, 20, 60, 90, 30, 20, 70, 90, 80, 50,
  70, 60, 90, 50, 30, 70, 20, 30, 60, 40, 30, 20, 60, 70, 80, 85,
  70, 70, 70, 50, 30, 40, 40, 40, 99, 80, 60, 60, 60, 60, 50, 70,
  10, 40, 70, 90, 80, 50, 40, 40, 60, 70, 30, 60, 30, 60, 80, 75,
  60, 70, 80, 30), diagtime = c(7, 5, 3, 9, 11, 5, 10, 29, 18,
  6, 4, 58, 4, 9, 11, 3, 9, 2, 4, 4, 3, 5, 14, 2, 3, 4, 12, 4,
  12, 2, 15, 2, 5, 3, 2, 25, 4, 1, 28, 8, 1, 7, 11, 4, 23, 19,
  10, 6, 2, 5, 4, 5, 3, 4, 16, 5, 15, 2, 12, 12, 5, 12, 2, 2, 8,
  11, 5, 8, 13, 12, 6, 3, 8, 1, 7, 3, 21, 3, 2, 6, 36, 13, 2, 28,
  7, 11, 13, 2, 13, 2, 22, 4, 2, 2, 36, 9, 11, 8, 3, 2, 4, 2, 2,
  1, 17, 87, 8, 2, 5, 3, 3, 5, 22, 3, 3, 5, 8, 4, 4, 3, 3, 4, 10,
  3, 4, 4, 15, 4, 12, 5, 11, 10, 1, 5, 18, 4, 3), age = c(69, 64,
  38, 63, 65, 49, 69, 68, 43, 70, 81, 63, 63, 52, 48, 61, 42, 35,
  63, 56, 55, 67, 63, 65, 46, 53, 69, 68, 43, 55, 42, 64, 65, 65,
  55, 66, 60, 67, 53, 62, 67, 72, 48, 68, 67, 61, 60, 62, 38, 50,
  63, 64, 43, 34, 66, 62, 52, 47, 63, 68, 45, 41, 66, 62, 60, 66,
  38, 53, 37, 54, 60, 48, 52, 70, 50, 62, 65, 58, 62, 64, 63, 58,
  64, 52, 35, 63, 70, 51, 40, 69, 36, 71, 62, 60, 44, 54, 66, 49,
  72, 68, 62, 71, 70, 61, 71, 59, 67, 60, 69, 57, 39, 62, 50, 43,
  70, 66, 61, 81, 58, 63, 60, 62, 42, 69, 63, 45, 68, 39, 66, 63,
  49, 64, 65, 64, 67, 65, 37), prior = c(0, 10, 0, 10, 10, 0, 10,
  0, 0, 0, 0, 10, 0, 10, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 10,
  0, 0, 10, 10, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 10,
  10, 0, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 0, 10, 0, 10, 0, 0,
  0, 0, 0, 10, 10, 10, 0, 0, 10, 0, 10, 0, 10, 0, 0, 0, 0, 0, 0,
  10, 0, 0, 10, 0, 10, 0, 10, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 10, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 10,
  0, 0, 0, 0, 10, 10, 0, 0, 10, 10, 0, 0, 10, 0, 0), celltype.squamous = c(1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), celltype.smallcell = c(0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0), celltype.adeno = c(0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0), celltype.large = c(0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1)), class = "data.frame", row.names = c("1", "2", "3", "4",
  "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
  "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37",
  "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48",
  "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
  "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81",
  "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92",
  "93", "94", "95", "96", "97", "98", "99", "100", "101", "102",
  "103", "104", "105", "106", "107", "108", "109", "110", "111",
  "112", "113", "114", "115", "116", "117", "118", "119", "120",
  "121", "122", "123", "124", "125", "126", "127", "128", "129",
  "130", "131", "132", "133", "134", "135", "136", "137"))
