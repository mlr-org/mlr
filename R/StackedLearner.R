#' @title Create a stacked learner object.
#'
#' @description A stacked learner uses predictions of several base learners and fits
#' a super learner using these predictions as features in order to predict the outcome.
#' The following stacking methods are available:
#'
#'  \describe{
#'   \item{\code{average}}{Averaging of base learner predictions without weights.}
#'   \item{\code{stack.nocv}}{Fits the super learner, where in-sample predictions of the base learners are used.}
#'   \item{\code{stack.cv}}{Fits the super learner, where the base learner predictions are computed
#'   by crossvalidated predictions (the resampling strategy can be set via the \code{resampling} argument).}
#'  }
#'
#' @param base.learners [(list of) \code{\link{Learner}}]\cr
#'   A list of learners created with \code{makeLearner}.
#' @param super.learner [\code{\link{Learner} | character(1)}]\cr
#'   The super learner that makes the final prediction based on the base learners.
#'   If you pass a string, the super learner will be created via \code{makeLearner}.
#'   Not used for \code{method = 'average'}. Default is \code{NULL}.
#' @param predict.type [\code{character(1)}]\cr
#'   Sets the type of the final prediction for \code{method = 'average'}.
#'   For other methods, the predict type should be set within \code{super.learner}.
#'   If the type of the base learner prediction, which is set up within \code{base.learners}, is
#'   \describe{
#'    \item{\code{"prob"}}{then \code{predict.type = 'prob'} will use the average of all
#'    bease learner predictions and \code{predict.type = 'response'} will use
#'    the class with highest probability as final prediction.}
#'    \item{\code{"response"}}{then, for classification tasks with \code{predict.type = 'prob'},
#'    the final prediction will be the relative frequency based on the predicted base learner classes
#'    and classification tasks with \code{predict.type = 'response'} will use majority vote of the base
#'    learner predictions to determine the final prediction.
#'    For regression tasks, the final prediction will be the average of the base learner predictions.}
#'   }
#'
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
  method = "stack.nocv", use.feat = FALSE, resampling = NULL) {

  if (is.character(base.learners)) base.learners = lapply(base.learners, checkLearner)
  if (!is.null(super.learner)) {
    super.learner = checkLearner(super.learner)
    if (!is.null(predict.type)) super.learner = setPredictType(super.learner, predict.type)
  }

  baseType = unique(extractSubList(base.learners, "type"))
  if (!is.null(resampling) & method != "stack.cv") {
    stop("No resampling needed for this method")
  }
  if (is.null(resampling)) {
    resampling = makeResampleDesc("CV", iters= 5L,
      stratify = ifelse(baseType == "classif", TRUE, FALSE))
  }
  assertChoice(method, c("average", "stack.nocv", "stack.cv"))
  assertClass(resampling, "ResampleDesc")

  pts = unique(extractSubList(base.learners, "predict.type"))
  if ("se"%in%pts | (!is.null(predict.type) && predict.type == "se") |
        (!is.null(super.learner) && super.learner$predict.type == "se"))
    stop("Predicting standard errors currently not supported.")
  if (length(pts) > 1L)
    stop("Base learner must all have the same predict type!")
  if (method == "average" & (!is.null(super.learner) | is.null(predict.type)) )
    stop("No super learner needed for this method or the 'predict.type' is not specified.")
  if (method != "average" & is.null(super.learner))
    stop("You have to specify a super learner for this method.")
  #if (method != "average" & !is.null(predict.type))
  #  stop("Predict type has to be specified within the super learner.")
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

# FIXME: see FIXME in predict.StackedLearner I don't know how to make it better.
#'
#' @title Returns the predictions for each base learner.
#'
#' @description Returns the predictions for each base learner.
#'
#' @param model [\code{WrappedModel}]\cr Wrapped model, result of train.
#' @param newdata [\code{data.frame}]\cr
#' New observations, for which the predictions using the specified base learners should be returned.
#' Default is \code{NULL} and extracts the base learner predictions that were made during the training.
#'
#' @details None.
#'
#' @export
#'
getStackedBaseLearnerPredictions = function(model, newdata = NULL) {
  # get base learner and predict type
  bms = model$learner.model$base.models
  method = model$learner.model$method

  if (is.null(newdata)) {
    probs = model$learner.model$pred.train
  } else {
    # if (model == "stack.cv") warning("Crossvalidated predictions for new data is not possible for this method.")
    # predict prob vectors with each base model
    probs = vector("list", length(bms))
    for (i in seq_along(bms)) {
      pred = predict(bms[[i]], newdata = newdata)
      probs[[i]] = getResponse(pred, full.matrix = ifelse(method == "average", TRUE, FALSE))
    }

    names(probs) = sapply(bms, function(X) X$learner$id) #names(.learner$base.learners)
  }
  return(probs)
}

#' @export
trainLearner.StackedLearner = function(.learner, .task, .subset, ...) {
  bls = .learner$base.learners
  ids = names(bls)
  # reduce to subset we want to train ensemble on
  .task = subsetTask(.task, subset = .subset)
  # init prob result matrix, where base learners store predictions
  probs = makeDataFrame(getTaskSize(.task), ncol = length(bls), col.types = "numeric",
    col.names = ids)
  switch(.learner$method,
    average = averageBaseLearners(.learner, .task),
    stack.nocv = stackNoCV(.learner, .task),
    stack.cv = stackCV(.learner, .task)
  )
}

# FIXME: if newdata is the same data that was also used by training, then getBaseLearnerPrediction
# won't use the crossvalidated predictions (for method = "stack.cv").
#' @export
predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) {
  use.feat = .model$learner$use.feat

  # get predict.type from learner and super model (if available)
  sm.pt = .model$learner$predict.type
  sm = .model$learner.model$super.model

  # get base learner and predict type
  bms.pt = unique(extractSubList(.model$learner$base.learners, "predict.type"))

  # get task information (classif)
  td = .model$task.desc
  levs = td$class.levels
  type = ifelse(td$type == "regr", "regr",
    ifelse(length(td$class.levels) == 2L, "classif", "multiclassif"))

  # predict prob vectors with each base model
  probs = getStackedBaseLearnerPredictions(model = .model, newdata = .newdata)

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
      if (type == "classif" || type == "multiclassif") {
        # if base learner predictions are responses for classification
        if (sm.pt == "prob") {
          # if super learner predictions should be probabilities, iter over rows to get proportions
          # FIXME: this is very slow + CUMBERSOME. we also do it in more places
          # we need a bbmisc fun for counting proportions in rows or cols
          probs = apply(probs, 1L, function(x) (table(factor(x, td$class.levels) )/length(x)))
          return(setColNames(t(probs), td$class.levels))
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
    feat = .newdata[, colnames(.newdata) %nin% td$target, drop = FALSE]

    if (use.feat) {
      predData = cbind(probs, feat)
    } else {
      predData = probs
    }

    pred = predict(sm, newdata = predData)
    if (sm.pt == "prob") {
      return(as.matrix(getPredictionProbabilities(pred, cl = td$class.levels)))
    } else {
      return(pred$data$response)
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

### helpers to implement different ensemble types ###

# super simple averaging of base-learner predictions without weights. we should beat this
averageBaseLearners = function(learner, task) {
  bls = learner$base.learners
  base.models = probs = vector("list", length(bls))
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    model = train(bl, task)
    base.models[[i]] = model
    #
    pred = predict(model, task = task)
    probs[[i]] = getResponse(pred, full.matrix = TRUE)
  }
  names(probs) = names(bls)
  list(method = "average", base.models = base.models, super.model = NULL,
       pred.train = probs)
}

# stacking where we predict the training set in-sample, then super-learn on that
stackNoCV = function(learner, task) {
  td = getTaskDescription(task)
  type = ifelse(td$type == "regr", "regr",
    ifelse(length(td$class.levels) == 2L, "classif", "multiclassif"))
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

  pred.train = probs

  if (type == "regr" | type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[,-ncol(X)]))
  }

  # now fit the super learner for predicted_probs --> target
  probs[[td$target]] = getTaskTargets(task)
  if (use.feat) {
    # add data with normal features
    feat = getTaskData(task)
    feat = feat[, colnames(feat) %nin% td$target, drop = FALSE]
    probs = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = probs,
      target = td$target)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = td$target)
  }
  super.model = train(learner$super.learner, super.task)
  list(method = "stack.no.cv", base.models = base.models,
       super.model = super.model, pred.train = pred.train)
}

# stacking where we crossval the training set with the base learners, then super-learn on that
stackCV = function(learner, task) {
  td = getTaskDescription(task)
  type = ifelse(td$type == "regr", "regr",
    ifelse(length(td$class.levels) == 2L, "classif", "multiclassif"))
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
  tn = getTaskTargetNames(task)
  test.inds = unlist(rin$test.inds)

  pred.train = as.list(probs[order(test.inds), , drop = FALSE])

  probs[[tn]] = getTaskTargets(task)[test.inds]

  # now fit the super learner for predicted_probs --> target
  probs = probs[order(test.inds), , drop = FALSE]
  if (use.feat) {
    # add data with normal features IN CORRECT ORDER
    feat = getTaskData(task)#[test.inds, ]
    feat = feat[, !colnames(feat)%in%tn, drop = FALSE]
    predData = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = predData, target = tn)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = tn)
  }
  super.model = train(learner$super.learner, super.task)
  list(method = "stack.cv", base.models = base.models,
       super.model = super.model, pred.train = pred.train)
}

### other helpers ###

# Returns response for correct usage in stackNoCV and stackCV and for predictions
getResponse = function(pred, full.matrix = TRUE) {
  # if classification with probabilities
  if (pred$predict.type == "prob") {
    if (full.matrix) {
      # return matrix of probabilities
      td = pred$task.desc
      predReturn = pred$data[, paste("prob", td$class.levels, sep = ".")]
      colnames(predReturn) = td$class.levels
      return(predReturn)
    } else {
      # return only vector of probabilities for binary classification
      return(getPredictionProbabilities(pred))
    }
  } else {
    # if regression task
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

# TODOs:
# - document + test + export
# - benchmark stuff on openml
# - allow base.learners to be character of learners (not only list of learners)
# - rename 'probs' in code into 'preds'
# - allow option to remove predictions for one class in multiclass tasks (to avoid collinearity)
# - DONE: return predictions from each single base learner
# - DONE: allow predict.type = "response" for classif using majority vote (for super learner predict type "response")
#   and using average for super learner predict type "prob".
# - DONE: add option to use normal features in super learner
# - DONE: super learner can also return predicted probabilites
# - DONE: allow regression as well
