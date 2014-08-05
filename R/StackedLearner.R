# TODOs Bernd:
# - document + test + export
# - do we really need probability predictions from classif base.learners? not really?
# - line  probs[,i] = getProbabilities(r$pred) in stackNoCV and stackCV does not work for multiclass
# - benchmark stuff on openml

# TODOs Giuseppe:
# - allow base.learners to be character of learners (not only list of learners)
# - return predictions from each single base learner
# - check if base learner type is equal to super learner type
# - rename probs into preds

# - DONE: add option to use normal features in super learner
# - DONE: super learner can also return predicted probabilites 
# - DONE: allow regression as well

makeStackedLearner = function(base.learners, super.learner, method = "stack.nocv", 
                              use.feat = FALSE, resampling = NULL) {
  
  baseType = unique(extractSubList(base.learners, "type"))
  if (is.null(resampling)) {
    resampling = makeResampleDesc("CV", iters= 5L, 
                                  stratify = ifelse(baseType == "classif", TRUE, FALSE))
  } 
  assertChoice(method, c("average", "stack.nocv", "stack.cv"))
  assertClass(resampling, "ResampleDesc")
  super.learner = checkLearner(super.learner)
  # get type from super learner
  # bls.type = super.learner$type
  
  lrn =  makeBaseEnsemble(
    id = "stack",
    base.learners = base.learners,
    super.learner = super.learner,
    cl = "StackedLearner"
  )
  lrn$fix.factors = TRUE
  lrn$use.feat = use.feat
  
  #  pts = unique(extractSubList(base.learners, "predict.type"))
  #   if (!identical(pts, "prob"))
  #     stop("Base learners must all predict probabilities!")
  #   if (super.learner$type != "classif")
  #     stop("Super learner must be classifier!")
  if (method == "average" & super.learner$predict.type == "prob")
    messagef("super learner is replaced by method average")
  #  stop("Currently predicting probabilities are not allowed for this method")
  if (!inherits(resampling, "CVDesc"))
    stop("Currently only CV is allowed for resampling!")
  if (use.feat & method == "average")
    stop("The original features can not be used for this method")
  
  lrn$method = method
  lrn$super.learner = super.learner
  lrn$resampling = resampling
  return(lrn)
}

trainLearner.StackedLearner = function(.learner, .task, .subset, ...) {
  bls = .learner$base.learners
  ids = names(bls)
  # reduce to subset we want to train ensemble on
  .task = subsetTask(.task, subset = .subset)
  # init prob result matrix, where base learners store predictions
  probs = makeDataFrame(.task$task.desc$size, ncol = length(bls), col.types = "numeric",
                        col.names = ids)
  switch(.learner$method,
         average = averageBaseLearners(.learner, .task, probs),
         stack.nocv = stackNoCV(.learner, .task, probs),
         stack.cv = stackCV(.learner, .task, probs)
  )
}

predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) {
  use.feat = .model$learner$use.feat
  bms = .model$learner.model$base.models
  sm = .model$learner.model$super.model
  levs = .model$task.desc$class.levels
  probs = makeDataFrame(nrow = nrow(.newdata), ncol = length(bms), col.types = "numeric",
                        col.names = names(.learner$base.learners))
  
  # predict prob vectors with each base model
  for (i in seq_along(bms)) {
    pred = predict(bms[[i]], newdata = .newdata)
    probs[,i] = getResponse(pred)
  }
  
  if (.learner$method == "average") {
    prob = rowMeans(probs)
    td = .model$task.desc
    if (td$type == "classif"){
      if (sm$predict.type == "prob") {
        y = matrix(0, ncol = 2L, nrow = nrow(.newdata))
        colnames(y) = levs
        y[,1L] = prob
        y[,2L] = 1-prob
        return(y)
      } else {
        return(factor(ifelse(prob > 0.5, td$positive, td$negative), td$class.levels))
      }
    } else {
      return(prob)
    }
  } else {
    # feed probs into super model and we are done
    if (missing(.newdata)) {
      feat = getTaskData(task)
      feat = feat[, !colnames(feat)%in%.model$task.desc$target, drop=FALSE]
    } else {
      feat = .newdata[, !colnames(.newdata)%in%.model$task.desc$target, drop=FALSE]
    }
    
    if (use.feat) {
      predData = cbind(probs, feat)
    } else {
      predData = probs
    }
    
    pred = predict(sm, newdata = predData)$data
    if (sm$learner$predict.type == "prob") {
      # return predicted probabilities from super learner
      predProb = as.matrix(subset(pred, select=-response))
      colnames(predProb) = levs
      return(predProb)
    } else {
      return(pred$response)
    }
    
  }
}

### helpers to implement different ensemble types ###

# super simple averaging of base-learner predictions without weights. we should beat this
averageBaseLearners = function(learner, task, probs) {
  bls = learner$base.learners
  base.models = vector("list", length(bls))
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    model = train(bl, task)
    base.models[[i]] = model
  }
  # replace super.learner because it is not needed, however, predict.type is kept
  sl = list(method = "averaged", 
            predict.type = learner$super.learner$predict.type)
  list(base.models = base.models, super.model = sl)
}

# stacking where we predict the training set in-sample, then super-learn on that
stackNoCV = function(learner, task, probs) {
  bls = learner$base.learners
  use.feat = learner$use.feat
  base.models = vector("list", length(bls))
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    model = train(bl, task)
    base.models[[i]] = model
    pred = predict(model, task = task)
    probs[,i] = getResponse(pred)
  }
  # now fit the super learner for predicted_probs --> target
  probs[[task$task.desc$target]] = getTaskTargets(task)
  if (use.feat) {
    # add data with normal features
    feat = getTaskData(task)
    feat = feat[, !colnames(feat)%in%task$task.desc$target, drop=FALSE]
    super.task = makeSuperLearnerTask(learner, data = cbind(probs, feat), 
                                      target = task$task.desc$target)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = task$task.desc$target)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

# stacking where we crossval the training set with the base learners, then super-learn on that
stackCV = function(learner, task, probs) {
  bls = learner$base.learners
  use.feat = learner$use.feat
  # cross-validate all base learners and get a prob vector for the whole dataset for each learner
  base.models = vector("list", length(bls))
  rin = makeResampleInstance(learner$resampling, task = task)
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    r = resample(bl, task, rin, show.info = FALSE)
    probs[,i] = getResponse(r$pred)
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, task)
  }
  # add true target column IN CORRECT ORDER
  tn = task$task.desc$target
  test.inds = unlist(rin$test.inds)
  probs[[tn]] = getTaskTargets(task)[test.inds]
  # now fit the super learner for predicted_probs --> target
  
  if (use.feat) {
    # add data with normal features IN CORRECT ORDER
    feat = getTaskData(task)[test.inds, ]
    feat = feat[, !colnames(feat)%in%tn, drop=FALSE]
    super.task = makeSuperLearnerTask(learner, data = cbind(probs, feat), target = tn)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = tn)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

getResponse = function(pred) {
  if (pred$task.desc$type == "classif") {
    getProbabilities(pred)
    # if multiclass get ...
    
  } else {
    pred$data$response
  }
}

makeSuperLearnerTask = function(learner, data, target) {
  if (learner$super.learner$type == "classif") {
    makeClassifTask(data = data, target = target)
  } else {
    makeRegrTask(data = data, target = target)
  }
}