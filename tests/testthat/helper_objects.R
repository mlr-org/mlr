data(Sonar, package = "mlbench", envir = environment())
data(BreastCancer, package = "mlbench", envir = environment())
data(spatial.task, package = "mlr", envir = environment())

binaryclass.df = Sonar
binaryclass.formula = Class~.
binaryclass.target = "Class"
binaryclass.train.inds = c(1:50, 100:150)
binaryclass.test.inds  = setdiff(seq_len(nrow(binaryclass.df)), binaryclass.train.inds)
binaryclass.train = binaryclass.df[binaryclass.train.inds, ]
binaryclass.test  = binaryclass.df[binaryclass.test.inds, ]
binaryclass.class.col = 61
binaryclass.class.levs = levels(binaryclass.df[, binaryclass.class.col])
binaryclass.task = makeClassifTask("binary", data = binaryclass.df, target = binaryclass.target)

binaryclass.spatial.df = spatial.task$env$data
coordinates = spatial.task$coordinates
binaryclass.spatial.formula = slides~.
binaryclass.spatial.target = "slides"
binaryclass.spatial.train.inds = c(1:300, 600:900)
binaryclass.spatial.test.inds  = setdiff(seq_len(nrow(binaryclass.spatial.df)), binaryclass.spatial.train.inds)
binaryclass.spatial.train = binaryclass.spatial.df[binaryclass.spatial.train.inds, ]
binaryclass.spatial.test  = binaryclass.spatial.df[binaryclass.spatial.test.inds, ]
binaryclass.spatial.class.col = 3
binaryclass.spatial.class.levs = levels(binaryclass.spatial.df[, binaryclass.spatial.class.col])
binaryclass.spatial.task = makeClassifTask("binary", data = binaryclass.spatial.df, target = binaryclass.spatial.target, coordinates = coordinates)

multiclass.df = iris
multiclass.formula = Species~.
multiclass.target = "Species"
multiclass.train.inds = c(1:30, 51:80, 101:130)
multiclass.test.inds  = setdiff(1:150, multiclass.train.inds)
multiclass.train = multiclass.df[multiclass.train.inds, ]
multiclass.test  = multiclass.df[multiclass.test.inds, ]
multiclass.class.col = 5
multiclass.task = makeClassifTask("multiclass", data = multiclass.df, target = multiclass.target)

multiclass.small.df = iris[c(1:3, 51:53, 101:103), ]
multiclass.small.formula = Species~.
multiclass.small.target = "Species"
multiclass.small.train.inds = c(1:2, 4:5, 7:8)
multiclass.small.test.inds  = setdiff(1:9, multiclass.small.train.inds)
multiclass.small.train = multiclass.small.df[multiclass.small.train.inds, ]
multiclass.small.test  = multiclass.small.df[multiclass.small.test.inds, ]
multiclass.small.class.col = 5
multiclass.small.task = makeClassifTask("multiclass", data = multiclass.small.df, target = multiclass.small.target)

multilabel.df = iris
multilabel.df[, "y1"] = rep(c(TRUE, FALSE), 75L)
multilabel.df[, "y2"] = rep(c(FALSE, TRUE), 75L)
multilabel.target = c("y1", "y2")
multilabel.train.inds = c(1:30, 51:80, 101:130)
multilabel.test.inds  = setdiff(1:150, multilabel.train.inds)
multilabel.train = multilabel.df[multilabel.train.inds, ]
multilabel.test  = multilabel.df[multilabel.test.inds, ]
multilabel.task = makeMultilabelTask("multilabel", data = multilabel.df, target = multilabel.target)
multilabel.formula.cbind = as.formula(paste("cbind(", paste(multilabel.target, collapse = ",", sep = " "), ")  ~ .", sep = ""))
multilabel.formula = as.formula(paste(paste(multilabel.target, collapse = "+"), "~."))
multilabel.small.inds = c(1, 52, 53, 123)

noclass.df = iris[, -5]
noclass.train.inds = c(1:30, 51:80, 101:130)
noclass.test.inds  = setdiff(1:150, noclass.train.inds)
noclass.train = noclass.df[noclass.train.inds, ]
noclass.test  = noclass.df[noclass.test.inds, ]
noclass.task = makeClusterTask("noclass", data = noclass.df)

data(BostonHousing, package = "mlbench", envir = environment())
regr.df = BostonHousing
regr.formula = medv ~ .
regr.target = "medv"
regr.train.inds = seq(1, 506, 7)
regr.test.inds  = setdiff(seq_len(nrow(regr.df)), regr.train.inds)
regr.train = regr.df[regr.train.inds, ]
regr.test  = regr.df[regr.test.inds, ]
regr.class.col = 14
regr.task = makeRegrTask("regrtask", data = regr.df, target = regr.target)

regr.small.df = BostonHousing[150:160, ]
regr.small.formula = medv ~ .
regr.small.target = "medv"
regr.small.train.inds = 1:7
regr.small.test.inds  = setdiff(seq_len(nrow(regr.small.df)), regr.small.train.inds)
regr.small.train = regr.small.df[regr.small.train.inds, ]
regr.small.test  = regr.small.df[regr.small.test.inds, ]
regr.small.class.col = 14
regr.small.task = makeRegrTask("regrtask", data = regr.small.df, target = regr.small.target)

regr.num.df = regr.df[, sapply(regr.df, is.numeric)]
regr.num.formula = regr.formula
regr.num.target = regr.target
regr.num.train.inds = regr.train.inds
regr.num.test.inds  = regr.test.inds
regr.num.train = regr.num.df[regr.num.train.inds, ]
regr.num.test  = regr.num.df[regr.num.test.inds, ]
regr.num.class.col = 13
regr.num.task = makeRegrTask("regrnumtask", data = regr.num.df, target = regr.num.target)

regr.na.num.df = regr.num.df[1:10, ]
regr.na.num.df[1, 1] = NA
regr.na.num.formula = regr.num.formula
regr.na.num.target = regr.num.target
regr.na.num.train.inds = regr.num.train.inds
regr.na.num.test.inds  = regr.num.test.inds
regr.na.num.train = regr.na.num.df[regr.na.num.train.inds, ]
regr.na.num.test  = regr.na.num.df[regr.na.num.test.inds, ]
regr.na.num.class.col = 13
regr.na.num.task = makeRegrTask("regrnanumdf", data = regr.na.num.df, target = regr.na.num.target)

getSurvData = function(n = 100, p = 10) {
  set.seed(1)
  beta = c(rep(1, 10), rep(0, p - 10))
  x = matrix(rnorm(n * p), n, p)
  colnames(x) = sprintf("x%01i", 1:p)
  real.time = - (log(runif(n))) / (10 * exp(drop(x %*% beta)))
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
surv.test.inds  = setdiff(seq_len(nrow(surv.df)), surv.train.inds)
surv.train = surv.df[surv.train.inds, ]
surv.test  = surv.df[surv.test.inds, ]
surv.task = makeSurvTask("survtask", data = surv.df, target = surv.target)
rm(getSurvData)

data("gunpoint.task", package = "mlr")
data("fuelsubset.task", package = "mlr")
fda.binary.gp.task = gunpoint.task
suppressMessages({gp = getTaskData(gunpoint.task, subset = seq_len(100), functionals.as = "dfcols")})
gp.fdf = makeFunctionalData(gp[, seq_len(51)], fd.features = list("fd" = 2:51))
fda.binary.gp.task.small = makeClassifTask(data = gp.fdf, target = "X1")
fda.regr.fs.task = fuelsubset.task

# nonsense fda multiclass task
fda.multiclass.df = iris
fda.multiclass.formula = Species~.
fda.multiclass.target = "Species"
fda.multiclass.train.inds = c(1:30, 51:80, 101:130)
fda.multiclass.test.inds  = setdiff(1:150, multiclass.train.inds)
fda.multiclass.train = multiclass.df[multiclass.train.inds, ]
fda.multiclass.test  = multiclass.df[multiclass.test.inds, ]
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
