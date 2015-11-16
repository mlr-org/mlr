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

surv.df = cbind(time = rexp(150, 1/20)+1, event = sample(c(TRUE, FALSE), 150, replace = TRUE), iris)
surv.formula = survival::Surv(time, event) ~ .
surv.target = c("time", "event")
surv.train.inds = c(1:30, 51:80, 101:130)
surv.test.inds  = setdiff(1:150, surv.train.inds)
surv.train = surv.df[surv.train.inds, ]
surv.test  = surv.df[surv.test.inds, ]
surv.task = makeSurvTask("survtask", data = surv.df, target = surv.target)

costsens.feat = iris
costsens.costs = matrix(runif(150L * 3L, min = 0, max = 1), 150L, 3L)
costsens.task = makeCostSensTask("costsens", data = costsens.feat, costs = costsens.costs)

black.xpath = "//svg:path[contains(@style, 'fill:rgb(0%,0%,0%);')]"
grey.xpath = "//svg:path[contains(@style, 'fill:rgb(89.803922%,89.803922%,89.803922%);')]"
red.xpath = "//svg:path[contains(@style, 'fill:rgb(97.254902%,46.27451%,42.745098%);')]"
blue.xpath = "//svg:path[contains(@style, 'fill:rgb(38.039216%,61.176471%,100%);')]"
green.xpath = "//svg:path[contains(@style, 'fill:rgb(0%,72.941176%,21.960784%);')]"
black.line.xpath = "//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(0%,0%,0%);')]"
blue.line.xpath = "//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(0%,74.901961%,76.862745%);')]"
red.line.xpath = "//svg:path[contains(@style, 'stroke-linecap:butt;stroke-linejoin:round;stroke:rgb(97.254902%,46.27451%,42.745098%);')]"
black.bar.xpath = "//svg:path[contains(@style, 'stroke:none;fill-rule:nonzero;fill:rgb(20%,20%,20%);fill-opacity:1')]"


#tiny datasets for testing
#features
var1 = c(1, 2, 3, 4)
var2 = c(3, 4, 1, 2)
#for regression
tar.regr = c(5, 10, 0, 5)
pred.art.regr = c(4, 11, 0, 4)
data.regr = data.frame(var1, var2, tar.regr)
task.regr = makeRegrTask(data = data.regr, target = "tar.regr")
lrn.regr = makeLearner("regr.rpart")
mod.regr = train(lrn.regr, task.regr)
pred.regr = predict(mod.regr, task.regr)
pred.regr$data$response = pred.art.regr  
#for multiclass
tar.classif = factor(c(1L, 2L, 0L, 1L))
pred.art.classif = factor(c(1L, 1L, 0L, 2L))
data.classif = data.frame(var1, var2, tar.classif)
task.classif = makeClassifTask(data = data.classif, target = "tar.classif")
lrn.classif = makeLearner("classif.rpart", predict.type = "prob")
mod.classif = train(lrn.classif, task.classif)
pred.classif = predict(mod.classif, task.classif)
pred.classif$data$response = pred.art.classif
#for binaryclass
tar.bin = factor(c(1L, 0L, 0L, 1L))
pred.art.bin = factor(c(1L, 1L, 0L, 0L))
data.bin = data.frame(var1, var2, tar.bin)
task.bin = makeClassifTask(data = data.bin, target = "tar.bin")
lrn.bin = lrn.classif
mod.bin = train(lrn.bin, task.bin)
pred.bin = predict(mod.bin, task.bin)
pred.bin$data$response = pred.art.bin
#for multilabel
tar1.multilabel = c(TRUE, FALSE, FALSE, TRUE)
tar2.multilabel = c(TRUE, TRUE, FALSE, TRUE)
pred.art.multilabel = cbind(c(TRUE, FALSE, FALSE, FALSE), c(FALSE, TRUE, FALSE, TRUE))
data.multilabel = data.frame(var1, var2, tar1.multilabel, tar2.multilabel)
label.names = c("tar1.multilabel", "tar2.multilabel")
task.multilabel = makeMultilabelTask(data = data.multilabel, target = label.names)
lrn.multilabel = makeLearner("multilabel.rFerns")
mod.multilabel = train(lrn.multilabel, task.multilabel)
pred.multilabel = predict(mod.multilabel, task.multilabel)
pred.multilabel$data[,4:5] = pred.art.multilabel
#for survival
time.surv = c(5, 10, 5, 10)
status.surv = c(TRUE, FALSE, TRUE, FALSE)
pred.art.surv = c(1, -1, 1, 1)
data.surv = data.frame(var1, var2, time.surv, status.surv)
tar.names = c("time.surv", "status.surv")
task.surv = makeSurvTask(data = data.surv, target = tar.names)
lrn.surv = makeLearner("surv.coxph")
mod.surv = train(lrn.surv, task.surv)
pred.surv = predict(mod.surv, task.surv)
pred.surv$data[,"response"] = pred.art.surv 
#for costsensitive
tar.costsens = factor(c("a", "b", "c", "a"))
pred.art.costsens = factor(c("a", "b", "c", "c"))
data.costsens = data.frame(var1, var2)
costs = matrix(c(0, 1, 2, 1, 0, 2, 1, 2, 0, 0, 2,1), nrow = 4L, byrow = TRUE)
colnames(costs) = levels(tar.costsens)
rownames(costs) = rownames(data.costsens)
task.costsens = makeCostSensTask(data = data.costsens, cost = costs)
lrn.costsens = makeLearner("classif.multinom", trace = FALSE)
lrn.costsens = makeCostSensWeightedPairsWrapper(lrn.costsens)
mod.costsens = train(lrn.costsens, task.costsens)
pred.costsens = predict(mod.costsens, task = task.costsens)
pred.costsens$data$response = pred.art.costsens
#for clustering
pred.art.cluster = c(1L, 1L, 2L, 1L)
data.cluster = data.frame(var1, var2)
task.cluster = makeClusterTask(data = data.cluster)
lrn.cluster = makeLearner("cluster.EM")
mod.cluster = train(lrn.cluster, task.cluster)
pred.cluster = predict(mod.cluster, task.cluster)
pred.cluster$data$response = pred.art.cluster