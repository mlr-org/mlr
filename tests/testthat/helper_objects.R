data(Sonar, package = "mlbench")
data(BreastCancer, package = "mlbench")

binaryclass.df = Sonar
binaryclass.formula = Class~.
binaryclass.target = "Class"
binaryclass.train.inds = c(1:50, 100:150)
binaryclass.test.inds  = setdiff(1:nrow(binaryclass.df), binaryclass.train.inds)
binaryclass.train = binaryclass.df[binaryclass.train.inds, ]
binaryclass.test  = binaryclass.df[binaryclass.test.inds, ]
binaryclass.class.col = 61
binaryclass.class.levs = levels(binaryclass.df[, binaryclass.class.col])
binaryclass.task = makeClassifTask("binary", data = binaryclass.df, target = binaryclass.target)

multiclass.df = iris
multiclass.formula = Species~.
multiclass.target = "Species"
multiclass.train.inds = c(1:30, 51:80, 101:130)
multiclass.test.inds  = setdiff(1:150, multiclass.train.inds)
multiclass.train = multiclass.df[multiclass.train.inds, ]
multiclass.test  = multiclass.df[multiclass.test.inds, ]
multiclass.class.col = 5
multiclass.task = makeClassifTask("multiclass", data = multiclass.df, target = multiclass.target)

multiclass.small.df = iris[c(1:3, 51:53, 101:103),]
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

noclass.df = iris[,-5]
noclass.train.inds = c(1:30, 51:80, 101:130)
noclass.test.inds  = setdiff(1:150, noclass.train.inds)
noclass.train = noclass.df[noclass.train.inds, ]
noclass.test  = noclass.df[noclass.test.inds, ]
noclass.task = makeClusterTask("noclass", data = noclass.df)

data(BostonHousing, package = "mlbench")
regr.df = BostonHousing
regr.formula = medv ~ .
regr.target = "medv"
regr.train.inds = seq(1, 506, 7)
regr.test.inds  = setdiff(1:nrow(regr.df), regr.train.inds)
regr.train = regr.df[regr.train.inds, ]
regr.test  = regr.df[regr.test.inds, ]
regr.class.col = 14
regr.task = makeRegrTask("regrtask", data = regr.df, target = regr.target)

regr.small.df = BostonHousing[150:160,]
regr.small.formula = medv ~ .
regr.small.target = "medv"
regr.small.train.inds = 1:7
regr.small.test.inds  = setdiff(1:nrow(regr.small.df), regr.small.train.inds)
regr.small.train = regr.small.df[regr.small.train.inds, ]
regr.small.test  = regr.small.df[regr.small.test.inds, ]
regr.small.class.col = 14
regr.small.task = makeRegrTask("regrtask", data = regr.small.df, target = regr.small.target)

regr.num.df = regr.df[,sapply(regr.df, is.numeric)]
regr.num.formula = regr.formula
regr.num.target = regr.target
regr.num.train.inds = regr.train.inds
regr.num.test.inds  = regr.test.inds
regr.num.train = regr.num.df[regr.num.train.inds, ]
regr.num.test  = regr.num.df[regr.num.test.inds, ]
regr.num.class.col = 13
regr.num.task = makeRegrTask("regrnumtask", data = regr.num.df, target = regr.num.target)

getSurvData = function(n = 100, p = 10) {
  set.seed(1)
  beta <- c(rep(1,10),rep(0,p-10))
  x <- matrix(rnorm(n*p),n,p)
  colnames(x) = sprintf("x%01i", 1:p)
  real.time <- -(log(runif(n)))/(10 * exp(drop(x %*% beta)))
  cens.time <- rexp(n,rate=1/10)
  status <- ifelse(real.time <= cens.time, TRUE, FALSE)
  obs.time <- ifelse(real.time <= cens.time,real.time,cens.time) + 1
  return(cbind(data.frame(time = obs.time, status = status), x))
}
surv.df = getSurvData()
surv.formula = survival::Surv(time, status) ~ .
surv.target = c("time", "status")
surv.train.inds = seq(1, floor(2/3 * nrow(surv.df)))
surv.test.inds  = setdiff(1:nrow(surv.df), surv.train.inds)
surv.train = surv.df[surv.train.inds, ]
surv.test  = surv.df[surv.test.inds, ]
surv.task = makeSurvTask("survtask", data = surv.df, target = surv.target)
rm(getSurvData)

costsens.feat = iris
costsens.costs = matrix(runif(150L * 3L, min = 0, max = 1), 150L, 3L)
costsens.task = makeCostSensTask("costsens", data = costsens.feat, costs = costsens.costs)

ns.svg = c(svg = "http://www.w3.org/2000/svg")

black.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(0%,0%,0%);')]"
grey.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(85.098039%,85.098039%,85.098039%);')]"
lightgrey.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(92.156863%,92.156863%,92.156863%);')]"
red.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(97.254902%,46.27451%,42.745098%);')]"
blue.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(38.039216%,61.176471%,100%);')]"
green.xpath = "/svg:svg//svg:path[contains(@style, 'fill:rgb(0%,72.941176%,21.960784%);')]"
black.line.xpath = "/svg:svg//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(0%,0%,0%);')]"
blue.line.xpath = "/svg:svg//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(0%,74.901961%,76.862745%);')]"
red.line.xpath = "/svg:svg//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(97.254902%,46.27451%,42.745098%);')]"
black.bar.xpath = "/svg:svg//svg:path[contains(@style, 'stroke:none;fill-rule:nonzero;fill:rgb(34.901961%,34.901961%,34.901961%);fill-opacity:1')]"
