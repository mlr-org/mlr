# Coerce a Registry as Used in UseCase_RandomForests to a BenchmarkResult(as in mlr)

RegistryToBMR = function(reg, learners, measures, imputeMissing = TRUE){
  
  # Get list of resample results
  res = reduceResultsList(reg, ids =  findDone(reg),
                          fun = function(job, res){
    tsk = res$resample.res$task.id
    lrn = res$resample.res$learner.id
    dat = res$resample.res
    lst = eval(parse(text = paste0("list(" , tsk, "= list(", lrn, "= dat))")))
  })
  
  allLrns = c()
  for(i in seq_along(learners)){allLrns =  c(allLrns, learners[[i]]$id)}
  nLrns = length(unique(allLrns))
  
  # Coerce to format list of tasks that contain ResampleResults of all learners for the task. 
  r = list()
  for(i in seq_along(res)){
    tnm = names(res[[i]])[1]
    lnm = names(res[[i]][[tnm]])[1]
    r[[tnm]][[lnm]] = res[[i]][[tnm]][[lnm]]
  }

  # Coerce to BMR
  newBMR = list(results = r, measures = measures, learners = learners)
  class(newBMR) = "BenchmarkResult"
  
  # If impute is FALSE delete all tasks where not every learner is present
  if(!imputeMissing){
    # Create copy
    rClean = newBMR$results
    # Get Number of learners:
    for(i in seq_along(r)){
      if(length(r[[i]]) < nLrns){
        print(paste0("Deleting task: ", names(r)[i]))
        rClean[[names(r)[i]]] = NULL
      }
    }
    # write into newBMR
    newBMR$results = rClean
  }
  
  # If impute == TRUE then impute a predefined ResampleResult  (all meas = 1, all times = 3600)
  if(imputeMissing){
    # Define functions needed
      imputeMissingResampRes = function(learner, task){
      load("impute.RData")
      imp$learner.id = learner
      imp$task.id = task
      return(imp)
    }
  
    imputeBMR = function(bmr){
      # Write imputed resample result
      for(i in seq_along(bmr$results)){
        if(length(bmr$results[[i]])< nLrns){
          missing = setdiff(allLrns, names(bmr$results[[i]]))
          for(j in seq_along(missing)){
            imp = imputeMissingResampRes(missing[[j]], names(bmr$results)[i])
            eval(parse(text = paste0("bmr$results[[", i ,"]][['", missing[[j]], "']] = imp")))
          }
        }
      }
      return(bmr)
    }
    # Call function
    newBMR = imputeBMR(newBMR)
    # end if(impute == TRUE)
  }
  
  # Return BMR
  return(newBMR)
}

# Call:
# RegistryToBMR(reg, lrn, measures)


## Old Stuff for debugging.

if(FALSE){
  #  Fehler sammeln!
  # Create BMR to see Structure
  tsk = list(iris.task, sonar.task)
  lrns = list(makeLearner("classif.rpart"),
            makeLearner("classif.nnet"))
  rdesc = makeResampleDesc("CV", iters = 10)
  meas = list(mmce, ber, timetrain, timepredict)
  bmr = benchmark(lrns, tsk, rdesc, meas)

  # Inspect structure
  str(bmr, max.level = 1)
  bmr$measures
  bmr$learners
  str(bmr$results, max.level = 1)
  str(bmr$results[[1]], max.level = 1)

  # test consitency
  res = reduceResultsList(reg, ids =  findDone(reg))
  
  for(i in seq_along(res)){
    z = c(z, res[[i]]$resample.res$task.id)
    t = c(t, res[[i]]$resample.res$learner.id)
  }
  length(unique(t))
  
  # get impute.RData
  if(FALSE){
    impute = resample(learner = makeLearner("classif.rpart"),task = iris.task,
                      resampling = makeResampleDesc("CV", iters = 10),
                      measures = list(mmce,ber,timetrain, timepredict))
    impute$measures.test$mmce = 1L
    impute$measures.test$ber = 1L
    impute$measures.test$timetrain = 3600L
    impute$measures.test$timepredict = 3600L
    impute$aggr = c(1L, 1L, 3600L, 3600L)
    names(impute$aggr) = c("mmce.test.mean","ber.test.mean","timetrain.test.mean","timepredict.test.mean")
    impute$pred = "imputed"
    impute$measures.train = data.frame(c("imputed"))
    impute$err.msgs = "imputed"
    impute$models  = "imputed"
    impute$runtime = 3600*10
    imp = impute
    save(imp, file = "impute.RData")
  }
  
  
  
  resample(learner = makeLearner("classif.obliqueRF"),
           task = toMlr(getOMLTask(3627))$mlr.task,
           measures = mmce,
           resampling = makeResampleDesc("CV", iters =10))
}
