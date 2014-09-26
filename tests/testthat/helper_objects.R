library(mlbench)
data(Sonar, BreastCancer)

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

noclass.df = iris[,-5]
noclass.train.inds = c(1:30, 51:80, 101:130)
noclass.test.inds  = setdiff(1:150, noclass.train.inds)
noclass.train = noclass.df[noclass.train.inds, ]
noclass.test  = noclass.df[noclass.test.inds, ]
noclass.task = makeClusterTask("noclass", data = noclass.df)

data(BostonHousing)
regr.df = BostonHousing
regr.formula = medv ~ .
regr.target = "medv"
regr.train.inds = seq(1, 506, 3)
regr.test.inds  = setdiff(1:nrow(regr.df), regr.train.inds)
regr.train = regr.df[regr.train.inds, ]
regr.test  = regr.df[regr.test.inds, ]
regr.task = makeRegrTask("regrtask", data = regr.df, target = regr.target)

regr.num.df = regr.df[,sapply(regr.df, is.numeric)]
regr.num.formula = regr.formula
regr.num.target = regr.target
regr.num.train.inds = regr.train.inds
regr.num.test.inds  = regr.test.inds
regr.num.train = regr.num.df[regr.num.train.inds, ]
regr.num.test  = regr.num.df[regr.num.test.inds, ]
regr.num.task = makeRegrTask("regrnumtask", data = regr.num.df, target = regr.num.target)

surv.df = cbind(time = rexp(150, 1/20)+1, event = sample(c(TRUE, FALSE), 150, replace = TRUE), iris)
surv.formula = Surv(time,event) ~ .
surv.target = c("time", "event")
surv.train.inds = c(1:30, 51:80, 101:130)
surv.test.inds  = setdiff(1:150, surv.train.inds)
surv.train = surv.df[surv.train.inds, ]
surv.test  = surv.df[surv.test.inds, ]
surv.task = makeSurvTask("survtask", data = surv.df, target = surv.target)

costsens.feat  = iris
costsens.costs = matrix(runif(150L * 3L, min = 0, max = 1), 150L, 3L)
costsens.task = makeCostSensTask("costsens", data = costsens.feat, costs = costsens.costs)
