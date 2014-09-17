library(devtools)
library(mlr)
#load_all("mlr")
library(colorspace)
library("parallelMap")
configureMlr(show.learner.output=FALSE,  on.learner.warning = "quiet", show.info = FALSE,
             on.par.without.desc = "quiet")
parallelStartMulticore(15)
set.seed(123)
task = sonar.task 

# settings
sm.pt = "prob"
bms.pt = "prob"
classifMeasure = list("auc" = mlr:::auc, "acc" = acc, "timetrain" = timetrain)
rin =  makeResampleInstance("Bootstrap", iters = 30, task = task)
methods = c("stack.cv", "stack.nocv", "average")
super = makeLearner("classif.randomForest", predict.type=sm.pt)

# omit "slow" classif learners
classl <- listLearners(obj="classif", properties=c("factors","prob"))
classl <- classl[!grepl(".crs|.km|.mob|.nnet|.rvm|.IBk|.boosting|.randomForestSRC|.mda|.qda|.rda",classl)]
#classl <- c("classif.kknn", "classif.ksvm", "classif.svm")

# fit base learners and select only the 50% best base learners
base = lapply(as.list(classl), function(X) makeLearner(X, predict.type=bms.pt))
baseMod <- vector("list", length(base))
for(i in 1:length(base)) {
  baseMod[[i]] <- mlr:::resample(base[[i]], task, resampling=rin, measures=classifMeasure) 
  cat(classl[i], " finished", fill=TRUE)
}

# determine best AUC models
baseModAUC <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"auc"]))
colnames(baseModAUC) <- sapply(base, function(X) X$id)
medAUC <- apply(baseModAUC, 2, median)

bestAUC <- baseModAUC[, medAUC > quantile(medAUC, 0.5)]
pdf("classifBaseLearner.pdf")
boxplot(baseModAUC, las=2, main = "AUC")
abline(h=quantile(medAUC, 0.5), col="gray")
dev.off()
base = lapply(as.list(colnames(bestAUC)), function(X) makeLearner(X, predict.type=bms.pt))

# Fit stacked learner without original features
slrn = lapply(methods, function(X) {
  if(X=="average") {
    lrn = makeStackedLearner(base.learners = base, method = X,
                             predict.type = sm.pt)
    #lrn = setPredictType(lrn, predict.type = sm.pt)
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
rmNull <- !sapply(slrnFeat, is.null)
names(slrnFeat) = methods[rmNull]
slrnFeat = slrnFeat[rmNull]

res = lapply(slrn, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure))
resFeat = lapply(slrnFeat, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure))
parallelStop()

resAUC = do.call("cbind", lapply(res, function(X) X$measures.test[,"auc"]))
resFeatAUC = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"auc"]))

pdf("classifStack_auc.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resAUC)), rep(hlc[2], ncol(resFeatAUC)), rep(hlc[3], ncol(bestAUC)) )
aucs = cbind(resAUC, resFeatAUC, bestAUC)
ind = order(apply(aucs, 2, median), decreasing=TRUE)
boxplot(aucs[, ind], las = 2, col=cols[ind], main="AUC")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

# lets take a look at ACC
baseModACC <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"acc"]))
colnames(baseModACC) <- colnames(baseModAUC)
medACC <- apply(baseModACC, 2, median)
bestACC <- baseModACC[, medACC > quantile(medACC, 0.5)]
resACC = do.call("cbind", lapply(res, function(X) X$measures.test[,"acc"]))
resFeatACC = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"acc"]))

pdf("classifStack_acc.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resACC)), rep(hlc[2], ncol(resFeatACC)), rep(hlc[3], ncol(bestACC)) )
accs = cbind(resACC, resFeatACC, bestACC)
ind = order(apply(accs, 2, median), decreasing=TRUE)
boxplot(accs[, ind], las = 2, col=cols[ind], main="ACC")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

save.image(file="classifStack.RData")