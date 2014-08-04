# TODOs:
# - document + test + export
# - do we really need probability predictions from classif base.learners? not really?
# - line  probs[,i] = getProbabilities(r$pred) in stackNoCV and stackCV does not work for multiclass
# - allow regression as well
# - benchmark stuff on openml

# - allow base.learners to be character of learners (not only list of learners)
# - return predictions from each single base learner

# - DONE: add option to use normal features in super learner

makeStackedLearner = function(base.learners, super.learner, method = "stack.nocv",
  resampling = makeResampleDesc("CV", iters= 5L, stratify = TRUE), use.feat=FALSE) {

  lrn =  makeBaseEnsemble(
    id = "stack",
    base.learners = base.learners,
    bls.type = "classif",
    cl = "StackedLearner"
  )
  lrn$fix.factors = TRUE
  lrn$use.feat = use.feat

  assertChoice(method, c("average", "stack.nocv", "stack.cv"))
  super.learner = checkLearner(super.learner)
  assertClass(resampling, "ResampleDesc")

  pts = unique(extractSubList(base.learners, "predict.type"))
  if (!identical(pts, "prob"))
    stop("Base learners must all predict probabilities!")
  if (super.learner$type != "classif")
    stop("Super learner must be classifier!")
  if (!inherits(resampling, "CVDesc"))
    stop("Currently only CV is allowed for resampling!")
  if (use.feat && method=="average")
    stop("You can not use the original features for this method")

  lrn$method = method
  lrn$super.learner = super.learner
  lrn$resampling = resampling
  return(lrn)
}

trainLearner.StackedLearner = function(.learner, .task, .subset,  ...) {
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
  probs = makeDataFrame(nrow = nrow(.newdata), ncol = length(bms), col.types = "numeric",
    col.names = names(.learner$base.learners))
  #probs = as.list(probs)
  
  # predict prob vectors with each base model
  for (i in seq_along(bms)) {
    pred = predict(bms[[i]], newdata = .newdata)
    probs[,i] = getProbabilities(pred)
  }
  
  #probs = as.data.frame(probs)
  if (.learner$method == "average") {
    prob = rowMeans(probs)
    td = .model$task.desc
    factor(ifelse(prob > 0.5, td$positive, td$negative), td$class.levels)
  } else {
    # feed probs into super model and we are done
    if(use.feat){
      if (missing(.newdata)) {
        feat = getTaskData(task)
        feat = feat[, !colnames(feat)%in%.model$task.desc$target, drop=FALSE]
      } else {
        feat = .newdata[,!colnames(.newdata)%in%.model$task.desc$target, drop=FALSE]
      }
      predict(.model$learner.model$super.model, newdata =  cbind(probs, feat))$data$response
    } else{
      predict(.model$learner.model$super.model, newdata = probs)$data$response
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
  list(base.models = base.models, super.model = NULL)
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
     probs[,i] = getProbabilities(pred)
  }
  # now fit the super learner for predicted_probs --> target
  probs[[task$task.desc$target]] = getTaskTargets(task)
  if(use.feat){
    feat = getTaskData(task)
    feat = feat[, !colnames(feat)%in%task$task.desc$target, drop=FALSE]
    super.task = makeClassifTask(data = cbind(probs, feat), target = task$task.desc$target)
  }else{
    super.task = makeClassifTask(data = probs, target = task$task.desc$target)
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
    probs[,i] = getProbabilities(r$pred)
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, task)
  }
  # add true target column IN CORRECT ORDER
  tn = task$task.desc$target
  test.inds = unlist(rin$test.inds)
  probs[[tn]] = getTaskTargets(task)[test.inds]
  # now fit the super learner for predicted_probs --> target
  
  if(use.feat){
    feat = getTaskData(task)
    feat = feat[, !colnames(feat)%in%tn, drop=FALSE]
    super.task = makeClassifTask(data = cbind(probs, feat[test.inds,]), target = tn)
  }else{
    super.task = makeClassifTask(data = probs, target = tn)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

