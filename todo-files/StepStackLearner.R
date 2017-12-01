#' @title Create a stacked learner object.
#'
#' @description A stacked learner uses predictions of several base learners and fits
#' a super learner using these predictions as features in order to predict the outcome.
#'
#' @param base.learners [(list of) \code{\link{Learner}}]\cr
#'   A list of learners created with \code{makeLearner}.
#' @param super.learner [\code{\link{Learner} | character(1)}]\cr
#'   The super learner. If you pass a string, the super learner will be created via \code{makeLearner}.
#'   Not used for \code{method = 'average'}. Default is \code{NULL}.
#' @param predict.type
#' @param method [\code{character(1)}]\cr
#'   \dQuote{average} for averaging the predictions of the base learners,
#'   \dQuote{stack.nocv} for building a super learner using the predictions of the base learners and
#'   \dQuote{stack.cv} for building a super learner using crossvalidated predictions of the base learners.
#'   Default is \dQuote{stack.nocv}.
#' @param use.feat [\code{logical(1)}]\cr
#'   Whether the original features should also be passed to the super learner.
#'   Not used for \code{method = 'average'}.
#'   Default is \code{FALSE}.
#' @param resampling [\code{\link{ResampleDesc}}]\cr
#'   Resampling strategy for \code{method = 'stack.cv'}.
#'   Currently only CV is allowed for resampling.
#'   The default \code{NULL} uses 5-fold CV.
#' @export
makeStackedLearner = function(base.learners, super.learner = NULL, predict.type = NULL,
                              method = "stack.nocv",
                              use.feat = FALSE, resampling = NULL) {
  
  baseType = unique(extractSubList(base.learners, "type"))
  if (!is.null(resampling) & method != "stack.cv") {
    stop("No resampling needed for this method")
  }
  if (is.null(resampling)) {
    resampling = makeResampleDesc("CV", iters= 5L,
                                  stratify = ifelse(baseType == "classif", TRUE, FALSE))
  }
  assertChoice(method, c("average", "stack.nocv", "stack.cv", "step.stack.nocv", "step.stack.cv"))
  assertClass(resampling, "ResampleDesc")
  
  pts = unique(extractSubList(base.learners, "predict.type"))
  if (length(pts) > 1L)
    stop("Base learner must all have the same predict type!")
  if (method == "average" & (!is.null(super.learner) | is.null(predict.type)) )
    stop("No super learner needed for this method or the 'predict.type' is not specified.")
  if (method != "average" & is.null(super.learner))
    stop("You have to specify a super learner for this method.")
  if (method != "average" & !is.null(predict.type))
    stop("Predict type has to be specified within the super learner.")
  if (method == "average" & use.feat)
    stop("The original features can not be used for this method")
  if (!inherits(resampling, "CVDesc"))
    stop("Currently only CV is allowed for resampling!")
  
  # lrn$predict.type is "response" by default change it using setPredictType
  lrn =  makeBaseEnsemble(
    id = "stack",
    base.learners = base.learners,
    cl = "StackedLearner"
  )
  
  # get predict.type from super learner or from predict.type
  if (!is.null(super.learner)) {
    super.learner = checkLearner(super.learner)
    lrn = setPredictType(lrn, predict.type = super.learner$predict.type)
  } else {
    lrn = setPredictType(lrn, predict.type = predict.type)
  }
  
  lrn$fix.factors.prediction = TRUE
  lrn$use.feat = use.feat
  
  lrn$method = method
  lrn$super.learner = super.learner
  lrn$resampling = resampling
  return(lrn)
}

#' @export
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
         stack.cv = stackCV(.learner, .task, probs),
         step.stack.nocv = stepStackNoCV(.learner, .task, probs),
         step.stack.cv = stepStackCV(.learner, .task, probs)
  )
}

#' @export
predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) {
  use.feat = .model$learner$use.feat
  
  # get predict.type from learner and super model (if available)
  sm.pt = .model$learner$predict.type
  sm = .model$learner.model$super.model
  
  # get base learner and predict type
  bms.pt = unique(extractSubList(.model$learner$base.learners, "predict.type"))
  
  # get task information (classif)
  levs = .model$task.desc$class.levels
  td = .model$task.desc
  type = ifelse(td$type == "regr", "regr",
                ifelse(length(td$class.levels) == 2L, "classif", "multiclassif"))
  
  # predict prob vectors with each base model
  # FIXME: does this work correctly for CV-methods?
  probs = exportPredictions(model = .model, newdata = .newdata,
                            full.matrix = ifelse(.learner$method == "average", TRUE, FALSE),
                            method = .learner$method)
  
  if (.learner$method == "average") {
    if (bms.pt == "prob") {
      # if base learner predictions are probabilities for classification
      prob = Reduce("+", probs) / length(probs) #rowMeans(probs)
      if (sm.pt == "prob") {
        # if super learner predictions should be probabilities
        return(as.matrix(prob))
      } else {
        # if super learner predictions should be responses
        return(factor(colnames(prob)[apply(as.matrix(prob), 1L, which.max)], td$class.levels))
      }
    } else {
      probs = as.data.frame(probs)
      # if base learner predictions are responses
      if (type == "classif" | type == "multiclassif") {
        # if base learner predictions are responses for classification
        if (sm.pt == "prob") {
          # if super learner predictions should be probabilities
          return(as.matrix(t(apply(probs, 1L, function(X) (table(factor(X, td$class.levels) )/length(X)) ))) )
        } else {
          # if super learner predictions should be responses
          return(factor(apply(probs, 1L, computeMode), td$class.levels))
        }
      }
      if (type == "regr") {
        # if base learner predictions are responses for regression
        prob = Reduce("+", probs) / length(probs) #rowMeans(probs)
        return(prob)
      }
    }
  } else {
    probs = as.data.frame(probs)
    # feed probs into super model and we are done
    feat = .newdata[, !colnames(.newdata)%in%.model$task.desc$target, drop = FALSE]
    
    if (use.feat) {
      predData = cbind(probs, feat)
    } else {
      predData = probs
    }
    
    pred = predict(sm, newdata = predData)$data
    if (sm.pt == "prob") {
      # return predicted probabilities from super learner
      predProb = as.matrix(subset(pred, select = -response))
      colnames(predProb) = levs
      return(predProb)
    } else {
      return(pred$response)
    }
  }
}

# Sets the predict.type for the super learner of a stacked learner
#' @export
setPredictType.StackedLearner = function(learner, predict.type) {
  lrn = setPredictType.Learner(learner, predict.type)
  lrn$predict.type = predict.type
  if ("super.learner"%in%names(lrn)) lrn$super.learner$predict.type = predict.type
  return(lrn)
}

# Returns predictions for each base learner (depending on selected method)
#' @export
exportPredictions = function(model, newdata, full.matrix = TRUE, method) {
  # get base learner and predict type
  bms = model$learner.model$base.models
  
  # predict prob vectors with each base model
  probs = vector("list", length(bms))
  if (grepl("step", method)) {
    for (i in seq_along(bms)) {
      pred = predict(bms[[i]], newdata = newdata)
      probs[[i]] = getResponse(pred, full.matrix =  full.matrix)
      # predictions of previous base learner are appended
      addCol = cbind(probs[[i]])
      colnames(addCol) =  bms[[i]]$learner$id
      newdata = cbind(newdata, addCol)
    }
  } else {
    for (i in seq_along(bms)) {
      pred = predict(bms[[i]], newdata = newdata)
      probs[[i]] = getResponse(pred, full.matrix =  full.matrix)
    }
  }
  
  names(probs) = sapply(bms, function(X) X$learner$id)#names(.learner$base.learners)
  return(probs)
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
  type = ifelse(task$task.desc$type == "regr", "regr",
                ifelse(length(task$task.desc$class.levels) == 2L, "classif", "multiclassif"))
  bls = learner$base.learners
  use.feat = learner$use.feat
  base.models = probs = vector("list", length(bls))
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    model = train(bl, task)
    base.models[[i]] = model
    pred = predict(model, task = task)
    probs[[i]] = getResponse(pred, full.matrix = FALSE)
  }
  names(probs) = names(bls)
  
  if (type == "regr" | type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[,-ncol(X)]))
  }
  
  # now fit the super learner for predicted_probs --> target
  probs[[task$task.desc$target]] = getTaskTargets(task)
  if (use.feat) {
    # add data with normal features
    feat = getTaskData(task)
    feat = feat[, !colnames(feat)%in%task$task.desc$target, drop = FALSE]
    probs = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = probs,
                                      target = task$task.desc$target)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = task$task.desc$target)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

# stacking where we crossval the training set with the base learners, then super-learn on that
stackCV = function(learner, task, probs) {
  type = ifelse(task$task.desc$type == "regr", "regr",
                ifelse(length(task$task.desc$class.levels) == 2L, "classif", "multiclassif"))
  bls = learner$base.learners
  use.feat = learner$use.feat
  # cross-validate all base learners and get a prob vector for the whole dataset for each learner
  base.models = probs = vector("list", length(bls))
  rin = makeResampleInstance(learner$resampling, task = task)
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    r = resample(bl, task, rin, show.info = FALSE)
    probs[[i]] = getResponse(r$pred, full.matrix = FALSE)
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, task)
  }
  names(probs) = names(bls)
  
  if (type == "regr" | type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[,-ncol(X)]))
  }
  
  # add true target column IN CORRECT ORDER
  tn = task$task.desc$target
  test.inds = unlist(rin$test.inds)
  probs[[tn]] = getTaskTargets(task)[test.inds]
  
  # now fit the super learner for predicted_probs --> target
  probs = probs[order(test.inds), , drop = FALSE]
  if (use.feat) {
    # add data with normal features IN CORRECT ORDER
    feat = getTaskData(task)#[test.inds, ]
    feat = feat[, !colnames(feat)%in%tn, drop = FALSE]
    probs = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = probs, target = tn)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = tn)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

# stepwise stacking, where we predict the training set in-sample and append the predictions
# from previous base learners into the task-data (after fitting each base learner)
stepStackNoCV = function(learner, task, probs) {
  type = ifelse(task$task.desc$type == "regr", "regr",
                ifelse(length(task$task.desc$class.levels) == 2L, "classif", "multiclassif"))
  if (type == "multiclassif") stop("Currently, this method does not support multiclass tasks")
  bls = learner$base.learners
  use.feat = learner$use.feat
  base.models = probs = vector("list", length(bls))
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    model = train(bl, task)
    base.models[[i]] = model
    pred = predict(model, task = task)
    probs[[i]] = getResponse(pred, full.matrix = FALSE)
    if (is.null(ncol(probs[[i]]))) {
      addCol = cbind(probs[[i]])
      colnames(addCol) =  names(bls)[[i]]
    } else {
      # FIXME: for multiclass, predictions are matrices
      addCol = probs[[i]]
      colnames(addCol) =  paste(names(bls)[[i]], colnames(addCol), sep = ".")
    }
    # append prediction of i-th base learner
    task = addFeature(task, add = addCol)
  }
  names(probs) = names(bls)
  
  if (type == "regr" | type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[,-ncol(X)]))
  }
  
  # now fit the super learner for predicted_probs --> target
  probs[[task$task.desc$target]] = getTaskTargets(task)
  if (use.feat) {
    # add data with normal features
    feat = getTaskData(task)
    #feat = feat[, !colnames(feat)%in%task$task.desc$target, drop = FALSE]
    #probs = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = probs,
                                      target = task$task.desc$target)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = task$task.desc$target)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

# stepwise stacking, where we crossval the training set and append the predictions
# from previous base learners into the task-data (after fitting each base learner)
stepStackCV = function(learner, task, probs) {
  type = ifelse(task$task.desc$type == "regr", "regr",
                ifelse(length(task$task.desc$class.levels) == 2L, "classif", "multiclassif"))
  if (type == "multiclassif") stop("Currently, this method does not support multiclass tasks")
  bls = learner$base.learners
  use.feat = learner$use.feat
  base.models = probs = vector("list", length(bls))
  
  rin = makeResampleInstance(learner$resampling, task = task)
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    r = resample(bl, task, rin, show.info = FALSE)
    probs[[i]] = getResponse(r$pred, full.matrix = FALSE)
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, task)
    # add predictions in correct order
    if (is.null(ncol(probs[[i]]))) {
      addCol = cbind(probs[[i]][order(unlist(rin$test.inds))])
      colnames(addCol) =  names(bls)[[i]]
    } else {
      # FIXME: for multiclass, predictions are matrices
      addCol = probs[[i]][order(unlist(rin$test.inds))]
      colnames(addCol) =  paste(names(bls)[[i]], colnames(addCol), sep = ".")
    }
    # append prediction of i-th base learner
    task = addFeature(task, add = addCol)
  }
  names(probs) = names(bls)
  
  if (type == "regr" | type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[,-ncol(X)]))
  }
  
  # add true target column IN CORRECT ORDER
  tn = task$task.desc$target
  test.inds = unlist(rin$test.inds)
  probs[[tn]] = getTaskTargets(task)[test.inds]
  
  # now fit the super learner for predicted_probs --> target
  probs = probs[order(test.inds), , drop = FALSE]
  if (use.feat) {
    # add data with normal features
    feat = getTaskData(task)
    #feat = feat[, !colnames(feat)%in%task$task.desc$target, drop = FALSE]
    #probs = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = probs,
                                      target = task$task.desc$target)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = task$task.desc$target)
  }
  super.model = train(learner$super.learner, super.task)
  list(base.models = base.models, super.model = super.model)
}

### other helpers ###

# adds a new predictor "add" into the task
addFeature = function(task, subset, features, add) {
  task = changeData(task, cbind(getTaskData(task, subset, features), add), getTaskCosts(task, subset))
  if (!missing(subset)) {
    if (task$task.desc$has.blocking)
      task$blocking = task$blocking[subset]
    if (task$task.desc$has.weights)
      task$weights = task$weights[subset]
  }
  return(task)
}

# Returns response for correct usage in stackNoCV and stackCV and for predictions
getResponse = function(pred, full.matrix = TRUE) {
  # If classification with probabilities
  if (pred$predict.type == "prob") {
    if (full.matrix) {
      # Return matrix of probabilities
      predReturn = pred$data[, paste("prob", pred$task.desc$class.levels, sep = ".")]
      colnames(predReturn) = pred$task.desc$class.levels
      return(predReturn)
    } else {
      # Return only vector of probabilities for binary classification
      getPredictionProbabilities(pred)
    }
  } else {
    # If regression task
    pred$data$response
  }
}

# Create a super learner task
makeSuperLearnerTask = function(learner, data, target) {
  if (learner$super.learner$type == "classif") {
    makeClassifTask(data = data, target = target)
  } else {
    makeRegrTask(data = data, target = target)
  }
}
# FIXMEs:
# - document + test + export
# - benchmark stuff on openml
# - allow base.learners to be character of learners (not only list of learners)
# - DONE: return predictions from each single base learner
# - rename probs into preds
# - DONE: allow predict.type = "response" for classif using majority vote (for super learner predict type "response")
#   and using average for super learner predict type "prob".
# - DONE: add option to use normal features in super learner
# - DONE: super learner can also return predicted probabilites
# - DONE: allow regression as well
