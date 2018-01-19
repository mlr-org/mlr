library(devtools)
library(mlr)
#load_all("mlr")
library(colorspace)
library("parallelMap")
configureMlr(show.learner.output=FALSE,  on.learner.warning = "quiet",
             on.par.without.desc = "quiet")
parallelStartMulticore(15)
set.seed(123)
library(gdata)
#http://archive.ics.uci.edu/ml/datasets/User+Knowledge+Modeling
# dat <- rbind(read.xls("http://archive.ics.uci.edu/ml/machine-learning-databases/00257/Data_User_Modeling_Dataset_Hamdi%20Tolga%20KAHRAMAN.xls",
#                  header=TRUE, sheet=2),
#              read.xls("http://archive.ics.uci.edu/ml/machine-learning-databases/00257/Data_User_Modeling_Dataset_Hamdi%20Tolga%20KAHRAMAN.xls",
#                       header=TRUE, sheet=3))
# dat <- dat[,1:6]
# dat$LPR <- NULL
# dat$X.UNS <- as.factor(gsub("very_l|Very L","L",as.character(dat$X.UNS)))

colClass <- rep("factor",10)
colClass[c(1,4)] <- "numeric"
dat <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data",
                 header=FALSE, colClasses=colClass)
task = makeClassifTask(data = dat, target = "V10")

# settings
sm.pt = "prob"
bms.pt = "prob"
classifMeasure = list("ber" = ber, "acc" = acc, "timetrain" = timetrain)
rin =  makeResampleInstance("Bootstrap", iters = 30, task = task)
methods = c("stack.cv", "stack.nocv", "average")
super = makeLearner("classif.rpart", predict.type=sm.pt)

# use "fast" classif learners
classl <- listLearners(obj="classif", properties=c("prob", "multiclass", "factors"))
classl <- classl[!grepl(".crs|.km|.mob|.rvm|.IBk|.boosting|.randomForestSRC|.mda|.ada|.qda|.rda",classl)]
#classl <- c("classif.randomForest", "classif.kknn", "classif.ksvm", "classif.svm")

# fit base learners 
base = lapply(as.list(classl), function(X) makeLearner(X, predict.type=bms.pt))
baseMod <- vector("list", length(base))
for(i in 1:length(base)) {
  baseMod[[i]] <- mlr:::resample(base[[i]], task, resampling=rin, measures=classifMeasure) 
  cat(classl[i], " finished", fill=TRUE)
}
names(baseMod) <- sapply(base, function(X) X$id)

# determine best base learners
baseModBER <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"ber"]))
medBER <- apply(baseModBER, 2, median)
bestBER <- baseModBER[,medBER < quantile(medBER, 0.5)]

baseModACC <- do.call("cbind", lapply(baseMod, function(X) X$measures.test[,"acc"]))
medACC <- apply(baseModACC, 2, median)
bestACC <- baseModACC[,medACC > quantile(medACC, 0.5)]

pdf("multiclassifBaseLearner_cmc_rp.pdf")
boxplot(baseModBER, las=2, main = "BER")
abline(h=quantile(medBER, 0.5), col="gray")
boxplot(baseModACC, las=2, main = "ACC")
abline(h=quantile(medACC, 0.5), col="gray")
dev.off()

base = lapply(as.list(colnames(bestBER)), function(X) makeLearner(X, predict.type=bms.pt))

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

res = lapply(slrn, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure) )
resFeat = lapply(slrnFeat, function(X) mlr:::resample(X, task, resampling=rin, measures=classifMeasure) )
parallelStop()

#lets take a look at BER
resBER = do.call("cbind", lapply(res, function(X) X$measures.test[,"ber"]))
resFeatBER = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"ber"]))
pdf("multiclassifStack_ber_cmc_rp.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resBER)), rep(hlc[2], ncol(resFeatBER)), rep(hlc[3], ncol(bestBER)) )
bers = cbind(resBER, resFeatBER, bestBER)
ind = order(apply(bers, 2, median), decreasing=TRUE)
boxplot(bers[, ind], las = 2, col=cols[ind], main="BER")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

#lets take a look at ACC
resACC = do.call("cbind", lapply(res, function(X) X$measures.test[,"acc"]))
resFeatACC = do.call("cbind", lapply(resFeat, function(X) X$measures.test[,"acc"]))
pdf("multiclassifStack_acc_cmc_rp.pdf")
par(mar=c(10,4,2,2))
hlc = rainbow_hcl(3)
cols = c(rep(hlc[1], ncol(resACC)), rep(hlc[2], ncol(resFeatACC)), rep(hlc[3], ncol(bestACC)) )
accs = cbind(resACC, resFeatACC, bestACC)
ind = order(apply(accs, 2, median), decreasing=TRUE)
boxplot(accs[, ind], las = 2, col=cols[ind], main="ACC")
legend("bottomleft", col=hlc, pch=15,
       legend = c("stack without features", "stack with features", "base learners"))
dev.off()

save.image(file="multiclassifStack_cmc_rp.RData")