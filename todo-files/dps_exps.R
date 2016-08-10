library(BatchExperiments)
library(OpenML)
library(mlr)
#library(parallelMap)
#parallelStartSocket(25)

# library(devtools)
# load_all()

datasets <- na.omit(listOMLDataSets())
datasets <- datasets[datasets$NumberOfSymbolicFeatures == 0 &
                       datasets$NumberOfMissingValues == 0 &
                       #datasets$NumberOfFeatures < 50 & #
                       datasets$NumberOfClasses == 2 &
                       datasets$NumberOfInstances < 5000 & #
                       datasets$NumberOfInstances > 0, ]

lapply( datasets$did , function(X) listOMLDataSetQualities(X) )

oml.data.ids = datasets$did #c(61)
problem.ids = paste0("d", oml.data.ids)

resample.sizes = setNames(floor(2/3*datasets$NumberOfInstances), problem.ids) #c(500L, 1000L)
folds = c(4L, 8L, 16L)
cv.reps = c(1L, 5L, 10L)
cv.stratifies = c(FALSE, TRUE)
repls = 100L

learner = "classif.randomForest"

unlink("dps-files-RF2", recursive = TRUE)
reg =  makeExperimentRegistry(id = "dpsRF2", work.dir="dps2", file.dir="dps2",
  packages = c("mlr", "OpenML")
)

for (i in seq_along(oml.data.ids)) {
  addProblem(reg, id = problem.ids[i],
    static = list(oml.data.id = oml.data.ids[i]),
    dynamic = function(static, resample.size) {
      library(mlr)
      oml.data = getOMLDataSet(static$oml.data.id)
      task = toMlr(oml.data)
      resample.set = sample(getTaskSize(task), resample.size, replace = FALSE)
      list(task = task, resample.set = resample.set)
    }
  )
}

estimPerf = function(static, dynamic, do.subset, resampling) {
  library(mlr)
  task = dynamic$task
  if (do.subset)
    task = subsetTask(task, dynamic$resample.set)
  lrn = makeLearner("classif.randomForest", predict.type="prob")
  res = resample(lrn, task, resampling, measures = list(mmce, auc, brier))
  return(list(mmce = res$aggr[[1L]], auc = res$aggr[[2L]], brier = res$aggr[[3L]], res = res))
}

addAlgorithm(reg, "cv", fun = function(static, dynamic, cv.reps, folds, cv.stratify) {
  library(mlr)
  if (cv.reps ==  1L)
    rdesc = makeResampleDesc("CV", iters = folds, stratify = cv.stratify)
  else
    rdesc = makeResampleDesc("RepCV", folds = folds, reps = cv.reps, stratify = cv.stratify)
  estimPerf(static, dynamic, do.subset = TRUE, resampling = rdesc)
})

addAlgorithm(reg, "dps", fun = function(static, dynamic, folds) {
  library(mlr)
  rdesc = makeResampleDesc("DPS", iters = folds)
  #rin = makeResampleInstance(rdesc, task = dynamic$task)
  estimPerf(static, dynamic, do.subset = TRUE, resampling = rdesc)
})

addAlgorithm(reg, "true", fun = function(static, dynamic, iters) {
  size = getTaskSize(dynamic$task)
  train.inds = dynamic$resample.set
  test.inds = setdiff(1:size, train.inds)
  rin = makeFixedHoldoutInstance(train.inds, test.inds, size)
  estimPerf(static, dynamic, do.subset = FALSE, resampling = rin)
})

pdes = lapply(problem.ids, function(id) makeDesign(id,
  exhaustive = list(resample.size = resample.sizes[id])))
ades.cv = makeDesign("cv", 
  exhaustive = list(folds = folds, cv.reps = cv.reps, cv.stratify = cv.stratifies))
ades.dps = makeDesign("dps", exhaustive = list(folds = folds))

addExperiments(reg, prob.des = pdes, algo.des = ades.cv, repls = repls)
addExperiments(reg, prob.des = pdes, algo.des = "true", repls = repls)
addExperiments(reg, prob.des = pdes, algo.des = ades.dps, repls = repls)

batchExport(reg, estimPerf = estimPerf)
#batchExpandGrid(reg)

submitJobs(reg)
waitForJobs(reg)
#parallelStop()

measure = c("mmce", "auc", "brier")
res1 = reduceResultsExperiments(reg, fun = function(job, res) res[measure])
res1[is.na(res1)] = -1
methodDPS = with(res1, paste0(algo, folds))
methodCV = with(res1, paste0(algo, folds, ", rep=", cv.reps, ", stratify=", cv.stratify))
methodTRUE = with(res1, paste0(algo))
res1$method = with(res1, 
  ifelse(algo=="dps", methodDPS, ifelse(algo=="true", methodTRUE, methodCV)))

library(plyr) 
res2 = ddply(res1, c(getResultVars(res1, "prob"), "repl"), function(d) {
  j = which(d$algo == "true")
  ptrue = setNames(as.numeric(d[j, measure]), measure) #setNames(lapply(as.list(measure), function(X) d[j, X]), measure)
  cbind(mmceDiff = d$mmce-ptrue["mmce"],
        aucDiff = d$auc-ptrue["auc"],
        brierDiff = d$brier-ptrue["brier"],
        stratify = d$cv.stratify, 
        method = d$method)
})
res2$mmceDiff <- as.numeric(as.character(res2$mmceDiff))
res2$aucDiff <- as.numeric(as.character(res2$aucDiff))
res2$brierDiff <- as.numeric(as.character(res2$brierDiff))

# MSE barplots separated for each dataset
resMSE = aggregate(cbind(mmceDiff, aucDiff, brierDiff)~method+stratify+prob, res2, 
                   function(d) mean(d^2))
MSEline = ddply(resMSE, "prob", summarise, minMMCE = min(mmceDiff[mmceDiff!=0]),
                minAUC = min(aucDiff[aucDiff!=0]),
                minBrier= min(brierDiff[brierDiff!=0]))
resMSE = join(resMSE, MSEline, by = "prob")

ggplot(aes(y = mmceDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE[resMSE$method!="true",]
) + geom_bar(stat="identity", position="dodge", color="black") + facet_grid(.~prob)+
  geom_hline(aes(yintercept=minMMCE)) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) 

ggplot(aes(y = aucDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE[resMSE$method!="true",]
) + geom_bar(stat="identity", position="dodge", color="black") + facet_grid(.~prob)+
  geom_hline(aes(yintercept=minAUC)) + coord_cartesian(ylim = c(0, 0.01)) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) 

ggplot(aes(y = brierDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE[resMSE$method!="true",]
) + geom_bar(stat="identity", position="dodge", color="black") + facet_grid(.~prob)+
  geom_hline(aes(yintercept=minBrier)) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) 


# MSE barplots averaged over each dataset
resMSE = aggregate(cbind(mmceDiff, aucDiff, brierDiff)~method+stratify, res2, 
                   function(d) mean(d^2))

resMSE <- transform(resMSE, method = reorder(method, mmceDiff))
ggplot(aes(y = mmceDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE
) + geom_bar(stat="identity", position="dodge", color="black") + 
  geom_hline(aes(yintercept=min(mmceDiff[mmceDiff!=0])))

resMSE <- transform(resMSE, method = reorder(method, aucDiff))
ggplot(aes(y = aucDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE#[resMSE$stratify!=1,]
) + geom_bar(stat="identity", position="dodge", color="black") + 
  geom_hline(aes(yintercept=min(aucDiff[aucDiff!=0]))) + coord_cartesian(ylim = c(0, 0.01))

resMSE <- transform(resMSE, method = reorder(method, brierDiff))
ggplot(aes(y = brierDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE
) + geom_bar(stat="identity", position="dodge", color="black") + 
  geom_hline(aes(yintercept=min(brierDiff[brierDiff!=0])))


# MSE boxplots
resMSE = aggregate(cbind(mmceDiff, aucDiff, brierDiff)~method+stratify+prob+repl, res2, 
                   function(d) mean(d^2))
ggplot(aes(y = mmceDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE
) + geom_boxplot() + geom_hline(aes(yintercept=0))  + 
  #geom_hline(aes(yintercept=median(mmceDiff))) +
  coord_cartesian(ylim = c(0, 0.002))


ggplot(aes(y = aucDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE
) + geom_boxplot() + geom_hline(aes(yintercept=0))  + 
  #geom_hline(aes(yintercept=median(aucDiff))) + 
  coord_cartesian(ylim = c(0, 0.002))


ggplot(aes(y = brierDiff, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
       data = resMSE
) + geom_boxplot() + geom_hline(aes(yintercept=0))  + 
  #geom_hline(aes(yintercept=median(brierDiff)))
  coord_cartesian(ylim = c(0, 0.001))


# 
# 
# res3 = aggregate(cbind(mmce, auc, brier)~prob+algo+cv.reps+cv.stratify+resample.size+folds, 
#                  data=res1, sd)
# res3$method = with(res3, 
#   as.factor(paste(algo, folds, " rep=", cv.reps, " stratify=", cv.stratify, sep="")))
# #colnames(res3)[colnames(res3)=="brier"]<-"sd"
# 
# res4 = aggregate(cbind(mmce, auc, brier)~method, data=res2, function(X) mean(abs(X)))
# #colnames(res4)[colnames(res4)=="bias"]<-"meanBias"
# res4
# 
# res5 = aggregate(cbind(mmce, auc, brier)~method+prob, data=res2, function(X) mean(abs(X)))
# #colnames(res5)[colnames(res5)=="bias"]<-"meanBias"
# res5
# 
# res6 = ddply(res1, c(getResultVars(res1, "prob"), "repl"), function(d) {
#   j = which(d$algo == "true")
#   ptrue = setNames(lapply(as.list(measure), function(X) d[j, X]), measure)
#   d$method = paste0(d$algo, d$folds, ", rep=", d$cv.reps, ", stratify=", d$cv.stratify)
# 
#   ddply(d, "method", summarise, mmce = ((mmce-ptrue[["mmce"]])),
#         auc = ((auc-ptrue[["auc"]])),
#         brier = ((brier-ptrue[["brier"]])))
#         #   summarise(d, mmce = ((mmce-ptrue[["mmce"]])^2),
# #             auc = ((auc-ptrue[["auc"]])^2),
# #             brier = ((brier-ptrue[["brier"]])^2),
# #             method = paste(algo, folds, ", rep=", cv.reps, ", stratify=", cv.stratify, sep=""))
# })
# res7 = aggregate(cbind(mmce, auc, brier)~method+prob, data=res6, function(X) mean((X)))
# 
# res5 = aggregate(cbind(mmce, auc, brier)~method+prob, data=res2, function(X) mean(abs(X)))
# 
# library(ggplot2)
# # mean absolute bias averaged over all datasets
# ggplot(aes(y = mmce, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res4
# ) + geom_bar(stat="identity", position="dodge", color="black") + 
#   geom_hline(aes(yintercept=min(mmce[mmce!=0])))
# 
# ggplot(aes(y = auc, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res4
# ) + geom_bar(stat="identity", position="dodge", color="black") + 
#   geom_hline(aes(yintercept=min(auc[auc!=0])))
# 
# ggplot(aes(y = brier, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res4
# ) + geom_bar(stat="identity", position="dodge", color="black") + 
#   geom_hline(aes(yintercept=min(brier[brier!=0])))
# 
# # bias aggregated over all datasets
# ggplot(aes(y = mmce, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# ggplot(aes(y = auc, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# ggplot(aes(y = brier, x = as.factor(method), fill = as.factor(gsub(",.*","",method))), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# 
# # bias separated over all datasets
# ggplot(aes(y = mmce, x = as.factor(prob), fill = method), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# ggplot(aes(y = auc, x = as.factor(prob), fill = method), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# ggplot(aes(y = brier, x = as.factor(prob), fill = method), 
#        data = res2
# ) + geom_boxplot() + geom_hline(aes(yintercept=0))
# 
# 
# # 
# ggplot(aes(y = mmce, x = as.factor(paste(prob)), fill = method), 
#        data = res3
# ) + geom_bar(stat="identity", position="dodge", color="black")
# 
# ggplot(aes(y = meanBias, x = as.factor(prob), fill = method), 
#        data = res5
# ) + geom_bar(stat="identity", position="dodge", color="black")
