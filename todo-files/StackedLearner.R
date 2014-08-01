# TODOs:
# - document
# - allow regression as well
# - add option to use normal features in super learner
# - benchmark stuff on openml

makeStackedLearner = function(base.learners, super.learner, method = "stack.nocv",
  resampling = makeResampleDesc("CV", iters= 5L, stratify = TRUE)) {

  lrn =  makeBaseEnsemble(
    id = "stack",
    base.learners = base.learners,
    bls.type = "classif",
    cl = "StackedLearner"
  )

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


  lrn$method = method
  lrn$super.learner = super.learner
  lrn$resampling = resampling
  return(lrn)
}

trainLearner.StackedLearner = function(.learner, .task, .subset,  ...) {
  tn = .task$task.desc$target
  bls = .learner$base.learners
  ids = extractSubList(bls, "id")
  sl = .learner$super.learner
  # reduce to subset we want to train ensemble on
  .task = subsetTask(.task, subset = .subset)
  # init prob result matrix, where base learners store predictions
  probs = as.data.frame(matrix(NA, nrow = .task$task.desc$size, ncol = length(bls)))
  colnames(probs) = ids
  switch(.learner$method,
    average = averageBaseLearners(.task, bls, sl, probs),
    stack.nocv = stackNoCV(.task, bls, sl, probs),
    stack.cv = stackCV(.task, bls, sl, probs)
  )
}

predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) {
  bms = .model$learner.model$base.models
  k = length(bms)
  probs = as.data.frame(matrix(NA, nrow = nrow(.newdata), ncol = k))
  colnames(probs) = extractSubList(.learner$base.learners, "id")

  # predict prob vectors with each base model
  for (i in 1:k) {
    pred = predict(bms[[i]], newdata = .newdata)
    probs[,i] = getProbabilities(pred)
  }

  if (.learner$method == "average") {
    prob = rowMeans(probs)
    td = .model$task.desc
    factor(ifelse(prob > 0.5, td$positive, td$negative), td$class.levels)
  } else {
    # feed probs into super model and we are done
    predict(.model$learner.model$super.model, newdata = probs)$data$response
  }
}

### helpers to implement different ensemble types ###

# super simple averaging of base-learner predictions without weights. we should beat this
average = function(task, base.learners, super.learner, probs) {
  base.models = vector("list", length(base.learners))
  for (i in seq_along(base.learners)) {
    bl = base.learners[[i]]
    model = train(bl, task)
    base.models[[i]] = model
  }
  list(base.models = base.models, super.model = NULL)
}

# stacking where we predict the training set in-sample, then super-learn on that
stackNoCV = function(task, base.learners, super.learner, probs) {
  base.models = vector("list", length(base.learners))
  for (i in seq_along(base.learners)) {
     bl = base.learners[[i]]
     model = train(bl, task)
     base.models[[i]] = model
     pred = predict(model, task = task)
     probs[,i] = getProbabilities(pred)
  }
  # now fit the super learner for predicted_probs --> target
  probs[[task$task.desc$target]] = getTaskTargets(task)
  super.task = makeClassifTask(data = probs, target = task$task.desc$target)
  super.model = train(super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

# stacking where we crossval the training set with the base learners, then super-learn on that
stackCV = function(task, base.learners, super.learner, probs) {

  k = length(bls)
  # #
  # cross-validate all base learners and get a prob vector for the whole dataset
  # for each learners
  probs = as.data.frame(matrix(0, nrow = .task$task.desc$size, ncol = k))
  # name probs cols with learner ids
  colnames(probs) = ids
  base.models = vector("list", k)
  for (i in 1:k) {
    bl = bls[[i]]
    r = resample(bl, .task, rin, show.info = FALSE)
    probs[,i] = getProbabilities(r$pred)
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, .task)
  }
  # add true target column IN CORRECT ORDER
  probs[[tn]] = getTaskTargets(.task)[inds]
  # now fit the super learner for predicted_probs --> target
  super.task = makeClassifTask(data = probs, target = tn)
  super.model = train(.learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
  #
}

