library(devtools)
library(mlr)
library(colorspace)
library("parallelMap")
configureMlr(show.learner.output=FALSE,  on.learner.warning = "quiet", show.info = FALSE,
             on.par.without.desc = "quiet")
parallelStartMulticore(15)
set.seed(123)
task = createDummyFeatures(bh.task, method="reference")

# settings
sm.pt = "response"
bms.pt = "response"
methods = c("stack.cv", "stack.nocv", "average")
super = makeLearner("regr.lm", predict.type=sm.pt)
regrMeasure = list("mse" = mlr:::mse, "mae" =  mlr:::mae, "timetrain" = timetrain)
rin =  makeResampleInstance("Bootstrap", iters = 30, task = task)

# omit "slow" regr learners
regl = listLearners(obj="regr", properties="factors")
regl = regl[!grepl(".crs|.km|.mob|.nnet|.gbm|.rvm|.IBk|.fnn|.randomForestSRC|.kknn", regl)]
#regl = c("regr.blackboost", "regr.earth", "regr.kknn", 
#          "regr.ksvm", "regr.svm")
  
# fit base learners 
base = lapply(as.list(regl), function(X) makeLearner(X, predict.type=bms.pt))
baseMod = vector("list", length(base))
for (i in 1:length(base)) {
  baseMod[[i]] = mlr:::resample(base[[i]], task, resampling=rin, 
                                 measures=regrMeasure)
  cat(regl[i], " finished", fill=TRUE)
}

# get MSE's 
baseModMSE = do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"mse"]))
colnames(baseModMSE) = sapply(base, function(X) X$id)
medMSE = apply(baseModMSE, 2, median)

pdf("regrBaseLearner.pdf")
boxplot(baseModMSE, las=2, main="MSE of all Base Learners")
abline(h=quantile(medMSE, 0.5), col="gray")
dev.off()

# Select only the 50% best base learners
bestBase = baseModMSE[, medMSE <= quantile(medMSE, 0.5)]
base = lapply(as.list(colnames(bestBase)), function(X) makeLearner(X, predict.type=bms.pt))

# Fit stacked learner without original features 
slrn = lapply(methods, function(X) {
  if(X=="average") {
    lrn = makeStackedLearner(base.learners = base, method = X,
                             predict.type = sm.pt)
    return(lrn)
  } else {
    makeStackedLearner(base.learners = base,
                       super.learner = super, 
                       method = X)
  }
  } )
names(slrn) = methods

# use original features
slrnFeat = lapply(methods, function(X) {
  if(X!="average") {
    makeStackedLearner(base.learners = base,
                       super.learner = super, 
                       method = X, use.feat = TRUE)
  }} )
rmNull = !sapply(slrnFeat, is.null)
names(slrnFeat) = methods[rmNull]
slrnFeat = slrnFeat[rmNull]

res = lapply(slrn, function(X) mlr:::resample(X, task, resampling=rin, measures=regrMeasure))
resFeat = lapply(slrnFeat, function(X) mlr:::resample(X, task, resampling=rin, measures=regrMeasure))
parallelStop()

# get MSEs
resMSE = do.call("cbind", lapply(res, function(X) X$measures.test[,"mse"]))
resFeatMSE = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"mse"]))

pdf("regrStack.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resMSE)), rep(hlc[2], ncol(resFeatMSE)), rep(hlc[3], ncol(bestBase)) )
mses = cbind(resMSE, resFeatMSE, bestBase)
ind = order(apply(mses, 2, median), decreasing=TRUE)
boxplot(mses[, ind], las = 2, col=cols[ind], main="MSE")
legend("topright", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

save.image(file="regrStack.RData")