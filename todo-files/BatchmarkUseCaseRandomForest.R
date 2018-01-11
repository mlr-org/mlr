# This code is a slightly slimer version of the batchmark.R file found
# in https://github.com/mlr-org/mlr/blob/master/todo-files/batchmark.R
# It implements connectivity to OpenML, by accepting oml.task.ids as input
# parameters. 
# Additionally I uploaded a BatchmarkToBMR function which converts the files containted in the registry
# to a BenchmarkResult as used in mlr. This function is more or less a hack and should by no means used 
# productively. It is highly unperformant aswell.

# define functions needed for batchmark

batchmark = function(reg, learners, oml.task.id, measures = NULL, repls = 1L, save.models = FALSE, overwrite = FALSE, pm.opts = list()) {
  
  
  BatchExperiments:::checkExperimentRegistry(reg)
  if ("mlr" %nin% names(reg$packages))
    stop("mlr is required on the slaves, please add mlr via 
         'addRegistryPackages'")
  
  learners = ensureVector(learners, 1L, cl = "Learner")
  assertList(learners, types = "Learner", min.len = 1L)
  learner.ids = vcapply(learners, "[[", "id")
  if (anyDuplicated(learner.ids))
    stop("Duplicated learner ids found")
  
  if (is.null(measures)) {
    measures = default.measures(tasks[[1L]])
  } else {
    measures = ensureVector(measures, 1L, "Measure")
    assertList(measures, types = "Measure", min.len = 1L)
  }
  
  assertCount(repls)
  assertFlag(save.models)
  assertFlag(overwrite)
  assertList(pm.opts, names = "named")
  
  
  # generate problems
  pdes = Map(
    function(id, oml.task.id, seed) {
      task = getTaskFun(oml.task.id)
      static = list(task = task)
      addProblem(reg, id, static = static, overwrite = overwrite, seed = seed)
      makeDesign(id)
    }
    ,id = paste0("t", oml.task.id), oml.task.id = oml.task.id,
    seed = reg$seed +  seq_along(tasks)
  )
  
  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, save.models, pm.opts)
    addAlgorithm(reg, id, apply.fun, overwrite = overwrite)
    makeDesign(id)
  }, id = learner.ids, learner = learners)
  
  # add experiments
  addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = repls,
                 skip.defined = overwrite)
}

getAlgoFun = function(lrn, measures, save.models, pm.opts) {
  force(lrn)
  force(measures)
  force(save.models)
  force(pm.opts)
  
  function(job, static, dynamic) {
    if (length(pm.opts) > 0L) {
      do.call(parallelStart, pm.opts)
      on.exit(parallelStop())
    }

    res = list(resample.res = resample(learner = lrn, task = static$task$task,
                                       static$task$rin, measures = measures))
    res = c(res, n = static$task$task$task.desc$size,
            p = sum(static$task$task$task.desc$n.feat),
            classes = length(static$task$task$task.desc$class.levels))
    res = c(res, static$task$task$task.desc$n.feat)
    if (save.models) c(list(resample = resample)) else res
  }
}

# Function to get Task by  oml.task.id from OpenML and convert to mlr task
# + resampling strategy for this task.

getTaskFun = function(oml.task.id) {
  configureMlr(on.learner.error = "warn") # Configures the behavior of the package
  oml.task = getOMLTask(task.id = oml.task.id)
  z = toMlr(oml.task) # convert oml task to mlr task
  task = z$mlr.task # task
  task$task.desc$id = paste0("t", oml.task.id)
  rin = z$mlr.rin
  return(list(task = task, rin = rin))
}

# Gets a list of learners.
GetLearnerList = function(learner){
  learner.list = lapply(as.list(learner), makeLearner)
  return(learner.list)
}



library(OpenML)
# get the taskids i want to run 
class.tasks = listOMLTasks(type = 1)

# Subset according to criteria
sel.tasks = subset(class.tasks,
                   NumberOfInstances >= 200 & NumberOfInstances <= 100000 &
                   NumberOfFeatures <= 500 &
                   NumberOfClasses <= 50 &
                   NumberOfMissingValues == 0 &
                   estimation_procedure == "10-fold Crossvalidation" &
                   evaluation_measures == "predictive_accuracy"
                   )

# order by size:
sel.tasks$dims = sel.tasks$NumberOfInstances * sel.tasks$NumberOfFeatures
sel.tasks = sel.tasks[order(sel.tasks$dims,
                            decreasing = FALSE),]

# remove duplicates:
sel.tasks = sel.tasks[!duplicated(sel.tasks$name),]

# remove error datasets 
# (bugged data or data that can not be properly read from OpenML)
sel.tasks = sel.tasks[-which(sel.tasks$did == 292),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 1004),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 183),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 373),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 316),]

# Too many factors:
# additionally remove: 1047, 825
# sel.tasks = sel.tasks[-which(sel.tasks$did == 1074),]
sel.tasks = sel.tasks[-which(sel.tasks$did == 825),]


#remove duplicate artificial data
rm = setdiff(grep("fri_c", sel.tasks$name), grep("fri_c[1-9]_1000_", sel.tasks$name))
sel.tasks = sel.tasks[-rm,]

# remove big datasets 
sel.tasks = sel.tasks[sel.tasks$dims < 10^6,]

sel.tasks = sel.tasks[sel.tasks$NumberOfClasses <= 2,]

# finally task ids
tasks = sel.tasks$task_id[1]



library(checkmate)
library(mlr)
library(BatchExperiments)
library(OpenML)


# source new learners (should be in mlr by now.)
# source("Florian2/learner/Rlearner_classif_ranger.R")
# source("Florian2/learner/Rlearner_classif_RRF.R")
# source("Florian2/learner/Rlearner_classif_obliqueRF.R")
# source("Florian2/learner/RLearner_classif_rotationForest.R")
# source("Florian2/learner/Rlearner_classif_randomUniformForest.R")
# source("Florian2/learner/Rlearner_classif_rfsrcSyn.R")
# crashes: source("Florian2/Rlearner_classif_wsrf.R")

# define a number of trees for all learners:
numTrees = 500L

# create the learner(s), measures + file learner.R with all learners
learners = list(makeBaggingWrapper(makeLearner("classif.rpart"), bw.iters = numTrees, bw.feats = 0.8),
                makeLearner("classif.randomForest", par.vals =  list(ntree = numTrees)),
                makeLearner("classif.rFerns", par.vals = list(ferns = numTrees)),
                makeLearner("classif.cforest", par.vals = list(ntree = numTrees)),
                makeLearner("classif.randomForestSRC", par.vals = list(ntree = numTrees)),
                makeLearner("classif.ranger", par.vals = list(num.trees = numTrees)),
                makeLearner("classif.RRF", par.vals = list(ntree = numTrees)),
                makeLearner("classif.obliqueRF", par.vals = list(ntree = numTrees)),
                makeLearner("classif.rotationForest", par.vals = list(L = numTrees)),
               # makeLearner("classif.randomUniformForest", par.vals = 
               # list(ntree = numTrees)),
                makeLearner("classif.randomForestSRCSyn", par.vals = list(ntree = numTrees))
                # crashes: ,makeLearner("classif.wsrf", par.vals = 
                # list(ntrees = numTrees)) 
)

measures = list(mmce, ber, timetrain, timepredict)



# delete registry
# unlink("UseCase_benchmark-files", recursive = TRUE)

# Create registry 
# reg = makeExperimentRegistry("UseCase_benchmark",
#                              packages = c("mlr","OpenML"),
#                              src.dirs = "Florian/learner/")

# write into registry
batchmark(reg, learners, tasks, measures, overwrite = TRUE, repls = 1L)


if (FALSE) {
  # Code in Order to Test / Evaluate the created registry.
  # execute
  testJob(reg, 998, external = TRUE)
  showStatus(reg)
  submitJobs(reg, 1:30)
  
  reg = loadRegistry("Florian2/UseCase_benchmark-files",
                     work.dir = "C:/Users/admin/Documents/laufzeitanalyse_BA_Schober")
  
  showStatus(reg)
  # Aggregated performance getter with additional info
  res_agg = reduceResultsExperiments(reg,
                                 fun = function(job, res) {
                                   r1 = as.list(res$resample.res$aggr)
                                   res$resample.res = NULL
                                   return(c(r1, res))
                                 })
  
  # Unaggregated performance getter
    res_all = reduceResults(reg,
                            ids = setdiff(getJobIds(reg), c(findExpired(reg),
                                                            findErrors(reg))),
                            init = data.frame(),
                            fun = function(aggr, job, res) {
      exp.settings = job[c("id", "prob.id", "algo.id", "repl")]
      exp.settings = as.data.frame(exp.settings)
      mt = res$resample.res$measures.test
      a = cbind(exp.settings, mt)
      aggr = rbind(aggr, a)
      return(aggr)
    })
    
    result = list(agg = res_agg, all = res_all)
    save(result, file = "UseCase_Results.RData")
    
    # alternatively: coerce to BMR (appended)
    source("BatchmarkToBMR.R")
    bmr = RegistryToBMR(reg, learners, measures, imputeMissing = TRUE)
    bmr

  # consistent?
  testBMRConsistency = function(bmr){
      z = c()
      for(i in seq_along(bmr$results)){
        z = c(z, length(bmr$results[[i]]))
      }
    if(unique(z) == max(z)){
      print("BMR is consistent")
    } else {
      print(paste0("unique numbers of learners", unique(z))) 
    }
  }
  testBMRConsistency(bmr)
}
