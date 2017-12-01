library(devtools)
library(mlr)
#load_all("mlr")
library(colorspace)
library("parallelMap")
configureMlr(show.learner.output=FALSE,  on.learner.warning = "quiet",
             on.par.without.desc = "quiet")
parallelStartMulticore(15)
set.seed(123)
task = iris.task 

# settings
sm.pt = "prob"
bms.pt = "prob"
classifMeasure = list("multiclass.auc" = multiclass.auc, 
                      "ber" = ber, "acc" = acc, "timetrain" = timetrain)
rin =  makeResampleInstance("Bootstrap", iters = 30, task = task)
methods = c("stack.cv", "stack.nocv", "average")
super = makeLearner("classif.gbm", predict.type=sm.pt)

# use "fast" classif learners
classl <- listLearners(obj="classif", properties=c("prob", "multiclass"))
classl <- classl[!grepl(".crs|.km|.mob|.nnet|.rvm|.IBk|.boosting|.randomForestSRC|.ada|.mda",classl)]
#classl <- c("classif.randomForest", "classif.kknn", "classif.ksvm", "classif.svm")

# fit base learners 
base = lapply(as.list(classl), function(X) makeLearner(X, predict.type=bms.pt))
baseMod <- vector("list", length(base))
for(i in 1:length(base)) {
  baseMod[[i]] <- mlr:::resample(base[[i]], task, resampling=rin, measures=classifMeasure) 
  cat(classl[i], " finished", fill=TRUE)
}

# determine best base learners based on multiclass AUC
baseModAUC <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"multiclass.auc"]))
colnames(baseModAUC) <- sapply(base, function(X) X$id)
minAUC <- apply(baseModAUC, 2, min)

bestBase <- baseModAUC[,minAUC > quantile(minAUC, 0.75)]
pdf("multiclassifBaseLearner.pdf")
boxplot(baseModAUC, las=2, main = "AUC")
abline(h=quantile(minAUC, 0.75), col="gray")
dev.off()
base = lapply(as.list(colnames(bestBase)), function(X) makeLearner(X, predict.type=bms.pt))

# Fit stacked learner without 
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

# use features
slrnFeat = lapply(methods, function(X) {
  if(X!="average") {
    makeStackedLearner(base.learners = base,
                       super.learner = super, 
                       method = X, use.feat = TRUE)
  }} )
rmNull <- !sapply(slrnFeat, is.null)
names(slrnFeat) = methods[rmNull]
slrnFeat = slrnFeat[rmNull]

res = lapply(slrn, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure, show.info = TRUE) )
resFeat = lapply(slrnFeat, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure, show.info = TRUE) )
parallelStop()

resAUC = do.call("cbind", lapply(res, function(X) X$measures.test[,"multiclass.auc"]))
resFeatAUC = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"multiclass.auc"]))

pdf("multiclassifStack_auc.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resAUC)), rep(hlc[2], ncol(resFeatAUC)), rep(hlc[3], ncol(bestBase)) )
aucs = cbind(resAUC, resFeatAUC, bestBase)
ind = order(apply(aucs, 2, median), decreasing=TRUE)
boxplot(aucs[, ind], las = 2, col=cols[ind], main="Multiclass AUC")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

# lets look at the ACC
baseModACC <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"acc"]))
colnames(baseModACC) <- colnames(baseModAUC)
minACC <- apply(baseModACC, 2, min)
bestBase2 <- baseModACC[, minACC > quantile(minACC, 0.5)]
resACC = do.call("cbind", lapply(res, function(X) X$measures.test[,"acc"]))
resFeatACC = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"acc"]))

pdf("multiclassifStack_acc.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resACC)), rep(hlc[2], ncol(resFeatACC)), rep(hlc[3], ncol(bestBase2)) )
accs = cbind(resACC, resFeatACC, bestBase2)
ind = order(apply(accs, 2, median), decreasing=TRUE)
boxplot(accs[, ind], las = 2, col=cols[ind], main="ACC")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

save.image(file="multiclassifStack.RData")