library(mlr)
library(OpenML)
#get the tasks for the experiment
tl = listOMLTasks()
# filter data sets and get appropriate data set IDs:
# classification
class.tasks = subset(tl, task_type == "Supervised Classification" & NumberOfFeatures < 15 &
  NumberOfFeatures > 3 & NumberOfInstances < 100 & NumberOfMissingValues == 0)
class.tasks$estimation_procedure
#binaryclass
bin.ids = subset(class.tasks, NumberOfClasses == 2L)$task_id
#multiclass
multiclass.ids = subset(class.tasks, NumberOfClasses > 2L & NumberOfClasses < 5L)$task_id
#regression
regr.ids = subset(tl, task_type == "Supervised Regression" &
  NumberOfFeatures < 15 & NumberOfFeatures > 3 & NumberOfInstances < 100 &
  NumberOfMissingValues == 0)$task_id
#a list of task indices for each type
ids = list(bin = bin.ids, multiclass = multiclass.ids, regr = regr.ids)
#draw from each task type
cases = 1L
set.seed(getOption("mlr.debug.seed"))
ids = as.vector(lapply(ids, sample, size = cases))
#get the tasks
tsks = lapply(ids, getOMLTask)
#convert them to mlr and get the task desc and the resample instances
tsks.mlr.info = sapply(tsks, convertOMLTaskToMlr, simplify = FALSE)
tsks.mlr = sapply(tsks.mlr.info, "[[", "mlr.task", simplify = FALSE)
rins = sapply(tsks.mlr.info, "[[", "mlr.rin", simplify = FALSE)

##create the learners
lrns = list(bin = list(cl = "classif.rpart", predict.type = "prob"),
	multiclass = list("classif.rpart", predict.type = "prob"),
	regr = list("regr.rpart"))
lrns = lapply(lrns, do.call, what = "makeLearner")
##upload learner and get implementation id
impl.ids = sapply(lrns, uploadOMLFlow, simplify = FALSE) 
##upload and get OML run
set.seed(getOption("mlr.debug.seed"))
#run the tasks
ran.tsks = mapply(runTaskMlr, tsks, lrns, SIMPLIFY = FALSE)
run.ids = mapply(uploadOMLRun, ran.tsks, impl.ids)
save.image("datasetup.RData")
#########################################################################
#wait a bit before executing further, it takes some time
#for the measures to be available
runs = sapply(run.ids, getOMLRun, simplify = FALSE, verbosity = 0L)
#get all the calculated measures and store in list:
oml.measures = list()
for ( i in seq_along(runs)) {
  oml.measures[[i]] <- runs[[i]]$output.data$evaluations[, c("name", "value")]
}
#we need lists for the mlr-measures we want
cv.measures = list(bin = list(acc, auc, tpr, f1, ppv),
  multiclass = list(acc, ber),
  regr = list(mae, rmse))
#lets resample
cv.res = mapply(resample, lrns, tsks.mlr, rins, cv.measures,
  show.info = FALSE, SIMPLIFY = FALSE)
#We need the weighted mean, mlr$aggr is averaged over folds regardless of size.
#Bernd, what's the name of the function, that should be doing that for me?
#I wasn't able to find it
#So I need the number of obs in each fold
fold.sizes = lapply(rins, "[[", "test.inds")
fold.sizes = lapply(fold.sizes, FUN = function(x) {
  lapply(x, length)
})
fold.sizes = lapply(fold.sizes, unlist)
#then calculate the measures
mlr.measures = sapply(cv.res, "[[", "measures.test")
mlr.measures = mapply(FUN = function(x, y) {
  x$fold.sizes = y
  x[, 1L] = NULL
  x
}, mlr.measures, fold.sizes)
mlr.measures = lapply(mlr.measures, FUN = function(x) {
  apply(x, 2L, weighted.mean, w = x$fold.sizes)
})
mlr.measures = lapply(mlr.measures, round, digits = 6L)
#ok now lets put this together to compare stuff:
#binary
measures.bin = data.frame(measure = character(5L), mlr.val = numeric(5L),
  oml.val = character(5L), stringsAsFactors = FALSE)
measures.bin[1L, ] = list("accuracy", mlr.measures[[1L]]["acc"],
  oml.measures[[1L]][11L, 2L])
measures.bin[2L, ] = c("auc", mlr.measures[[1L]]["auc"],
  oml.measures[[1L]][1L, 2L])
measures.bin[3L, ] = c("recall", mlr.measures[[1L]]["tpr"],
  oml.measures[[1L]][13L, 2L])
measures.bin[4L, ] = c("fmeasure", mlr.measures[[1L]]["f1"],
  oml.measures[[1L]][4L, 2L])
measures.bin[5L, ] = c("precision", mlr.measures[[1L]]["ppv"],
  oml.measures[[1L]][10L, 2L])
#regr
measures.regr = data.frame(measure = character(2L), mlr.val = numeric(2L),
  oml.val = character(2L), stringsAsFactors = FALSE)
measures.regr[1L, ] = list("mae", mlr.measures[[3L]]["mae"],
  oml.measures[[3L]][1L, 2L])
measures.regr[2L, ] = c("rmse", mlr.measures[[3L]]["rmse"],
  oml.measures[[3L]][5L, 2L])
#ok, see what we got here
measures.bin
measures.regr

#Firstly:
#In OpenML measures for a specific class are calculated for each class
#and then averaged with classweights
#For example: Reproduce recall in measures.bin:
conf.mat = getConfMatrix(cv.res[[1L]]$pred)
class.weights = c(conf.mat[1L, 1L] + conf.mat[1L, 2L],
  conf.mat[2L, 1L] + conf.mat[2L, 2L])
recall1 = conf.mat[1L, 1L] / class.weights[1L]
recall2 = conf.mat[2L, 2L] / class.weights[2L]
mean.weighted.recall = weighted.mean(c(recall1, recall2), w = class.weights)
mean.weighted.recall