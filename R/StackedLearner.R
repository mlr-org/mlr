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
#'   \item{\code{hill.climb}}{Select a subset of base learner predictions by hill climbing algorithm.}
#'   \item{\code{compress}}{Train a neural network to compress the model from a collection of base learners.}
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
#'   \dQuote{stack.nocv} for building a super learner using the predictions of the base learners,
#'   \dQuote{stack.cv} for building a super learner using crossvalidated predictions of the base learners.
#'   \dQuote{hill.climb} for averaging the predictions of the base learners, with the weights learned from
#'   hill climbing algorithm and
#'   \dQuote{compress} for compressing the model to mimic the predictions of a collection of base learners
#'   while speeding up the predictions and reducing the size of the model.
#'   Default is \dQuote{stack.nocv},
#' @param use.feat [\code{logical(1)}]\cr
#'   Whether the original features should also be passed to the super learner.
#'   Not used for \code{method = 'average'}.
#'   Default is \code{FALSE}.
#' @param resampling [\code{\link{ResampleDesc}}]\cr
#'   Resampling strategy for \code{method = 'stack.cv'}.
#'   Currently only CV is allowed for resampling.
#'   The default \code{NULL} uses 5-fold CV.
#' @param parset the parameters for \code{hill.climb} method, including
#' \describe{
#'   \item{\code{replace}}{Whether a base learner can be selected more than once.}
#'   \item{\code{init}}{Number of best models being included before the selection algorithm.}
#'   \item{\code{bagprob}}{The proportion of models being considered in one round of selection.}
#'   \item{\code{bagtime}}{The number of rounds of the bagging selection.}
#'   \item{\code{metric}}{The result evaluation metric function taking two parameters \code{pred} and \code{true},
#'   the smaller the score the better.}
#' }
#' the parameters for \code{compress} method, including
#' \describe{
#'    \item{k}{the size multiplier of the generated data}
#'    \item{prob}{the probability to exchange values}
#'    \item{s}{the standard deviation of each numerical feature}
#' }
#' @examples
#'   # Classification
#'   data(iris)
#'   tsk = makeClassifTask(data = iris, target = "Species")
#'   base = c("classif.rpart", "classif.lda", "classif.svm")
#'   lrns = lapply(base, makeLearner)
#'   lrns = lapply(lrns, setPredictType, "prob")
#'   m = makeStackedLearner(base.learners = lrns,
#'     predict.type = "prob", method = "hill.climb")
#'   tmp = train(m, tsk)
#'   res = predict(tmp, tsk)
#'
#'   # Regression
#'   data(BostonHousing, package = "mlbench")
#'   tsk = makeRegrTask(data = BostonHousing, target = "medv")
#'   base = c("regr.rpart", "regr.svm")
#'   lrns = lapply(base, makeLearner)
#'   m = makeStackedLearner(base.learners = lrns,
#'     predict.type = "response", method = "compress")
#'   tmp = train(m, tsk)
#'   res = predict(tmp, tsk)
#' @export
makeStackedLearner = function(base.learners, super.learner = NULL, predict.type = NULL,
  method = "stack.nocv", use.feat = FALSE, resampling = NULL, parset = list()) {

  if (is.character(base.learners)) base.learners = lapply(base.learners, checkLearner)
  if (is.null(super.learner) && method == "compress") {
    super.learner = makeLearner(stri_paste(base.learners[[1]]$type, ".nnet"))
  }
  if (!is.null(super.learner)) {
    super.learner = checkLearner(super.learner)
    if (!is.null(predict.type)) super.learner = setPredictType(super.learner, predict.type)
  }

  base.type = unique(extractSubList(base.learners, "type"))
  if (!is.null(resampling) & method != "stack.cv") {
    stop("No resampling needed for this method")
  }
  if (is.null(resampling)) {
    resampling = makeResampleDesc("CV", iters = 5L,
      stratify = ifelse(base.type == "classif", TRUE, FALSE))
  }
  assertChoice(method, c("average", "stack.nocv", "stack.cv", "hill.climb", "compress"))
  assertClass(resampling, "ResampleDesc")

  pts = unique(extractSubList(base.learners, "predict.type"))
  if ("se" %in% pts || (!is.null(predict.type) && predict.type == "se") ||
        (!is.null(super.learner) && super.learner$predict.type == "se"))
    stop("Predicting standard errors currently not supported.")
  if (length(pts) > 1L)
    stop("Base learner must all have the same predict type!")
  if ((method == "average" || method == "hill.climb") & (!is.null(super.learner) || is.null(predict.type)))
    stop("No super learner needed for this method or the 'predict.type' is not specified.")
  if (method != "average" & method != "hill.climb" & is.null(super.learner))
    stop("You have to specify a super learner for this method.")
  #if (method != "average" & !is.null(predict.type))
  #  stop("Predict type has to be specified within the super learner.")
  if ((method == "average" || method == "hill.climb") & use.feat)
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
  lrn$parset = parset
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
getStackedBaseLearnerPredictions = function(model, newdata = NULL) {
  # get base learner and predict type
  bms = model$learner.model$base.models
  method = model$learner.model$method

  if (is.null(newdata) || ncol(newdata) == 0) {
    probs = model$learner.model$pred.train
  } else {
    # if (model == "stack.cv") warning("Crossvalidated predictions for new data is not possible for this method.")
    # predict prob vectors with each base model
    probs = vector("list", length(bms))
    for (i in seq_along(bms)) {
      pred = predict(bms[[i]], newdata = newdata)
      probs[[i]] = getResponse(pred, full.matrix = ifelse(method %in% c("average", "hill.climb"), TRUE, FALSE))
    }

    names(probs) = sapply(bms, function(X) X$learner$id) #names(.learner$base.learners)
  }
  return(probs)
}

#' @export
trainLearner.StackedLearner = function(.learner, .task, .subset, ...) {
  # reduce to subset we want to train ensemble on
  .task = subsetTask(.task, subset = .subset)
  switch(.learner$method,
    average = averageBaseLearners(.learner, .task),
    stack.nocv = stackNoCV(.learner, .task),
    stack.cv = stackCV(.learner, .task),
    # hill.climb = hillclimbBaseLearners(.learner, .task, ...)
    hill.climb = do.call(hillclimbBaseLearners, c(list(.learner, .task), .learner$parset)),
    compress = compressBaseLearners(.learner, .task, .learner$parset)
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
  type = checkStackSupport(td)

  # predict prob vectors with each base model
  if (.learner$method != "compress") {
    probs = getStackedBaseLearnerPredictions(model = .model, newdata = .newdata)
  } else {
    probs = .newdata
  }

  if (.learner$method %in% c("average", "hill.climb")) {
    if (.learner$method == "hill.climb") {
      model.weight = .model$learner.model$weights
    } else {
      model.weight = rep(1 / length(probs), length(probs))
    }
    if (bms.pt == "prob") {
      # if base learner predictions are probabilities for classification
      for (i in seq_along(probs))
        probs[[i]] = probs[[i]] * model.weight[i]
      prob = Reduce("+", probs)
      if (sm.pt == "prob") {
        # if super learner predictions should be probabilities
        return(as.matrix(prob))
      } else {
        # if super learner predictions should be responses
        return(factor(colnames(prob)[max.col(prob)], td$class.levels))
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
          #probs = apply(probs, 1L, function(x) (table(factor(x, td$class.levels))/length(x)))
          #return(setColNames(t(probs), td$class.levels))
          probs = rowiseRatio(probs, td$class.levels, model.weight)
          return(probs)
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
  } else if (.learner$method == "compress") {
    probs = as.data.frame(probs)
    pred = predict(sm, newdata = probs)
    if (sm.pt == "prob") {
      return(as.matrix(getPredictionProbabilities(pred, cl = td$class.levels)))
    } else {
      return(pred$data$response)
    }
  } else {
    probs = as.data.frame(probs)
    # feed probs into super model and we are done
    feat = .newdata[, colnames(.newdata) %nin% td$target, drop = FALSE]

    if (use.feat) {
      pred.data = cbind(probs, feat)
    } else {
      pred.data = probs
    }

    pred = predict(sm, newdata = pred.data)
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
  if ("super.learner" %in% names(lrn)) lrn$super.learner$predict.type = predict.type
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
  td = getTaskDesc(task)
  type = checkStackSupport(td)
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

  if (type == "regr" || type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[, -ncol(X)]))
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
  td = getTaskDesc(task)
  type = checkStackSupport(td)
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

  if (type == "regr" || type == "classif") {
    probs = as.data.frame(probs)
  } else {
    probs = as.data.frame(lapply(probs, function(X) X)) #X[, -ncol(X)]))
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
    feat = feat[, !colnames(feat) %in% tn, drop = FALSE]
    pred.data = cbind(probs, feat)
    super.task = makeSuperLearnerTask(learner, data = pred.data, target = tn)
  } else {
    super.task = makeSuperLearnerTask(learner, data = probs, target = tn)
  }
  super.model = train(learner$super.learner, super.task)
  list(method = "stack.cv", base.models = base.models,
       super.model = super.model, pred.train = pred.train)
}

hillclimbBaseLearners = function(learner, task, replace = TRUE, init = 0, bagprob = 1, bagtime = 1,
  metric = NULL, ...) {

  assertFlag(replace)
  assertInt(init, lower = 0)
  assertNumber(bagprob, lower = 0, upper = 1)
  assertInt(bagtime, lower = 1)

  td = getTaskDesc(task)
  type = checkStackSupport(td)
  if (is.null(metric)) {
    if (type == "regr") {
      metric = function(pred, true) mean((pred - true)^2)
    } else {
      metric = function(pred, true) {
        pred = colnames(pred)[max.col(pred)]
        tb = table(pred, true)
        return(1 - sum(diag(tb)) / sum(tb))
      }
    }
  }
  assertFunction(metric)

  bls = learner$base.learners
  if (type != "regr" && type != "fcregr" && type != "mfcregr") {
    for (i in seq_along(bls)) {
      if (bls[[i]]$predict.type == "response")
        stop("Hill climbing algorithm only takes probability predict type for classification.")
    }
  }
  # cross-validate all base learners and get a prob vector for the whole dataset for each learner
  base.models = probs = vector("list", length(bls))
  rin = makeResampleInstance(learner$resampling, task = task)
  for (i in seq_along(bls)) {
    bl = bls[[i]]
    r = resample(bl, task, rin, show.info = FALSE)
    if (type == "regr") {
      probs[[i]] = matrix(getResponse(r$pred), ncol = 1)
    } else {
      probs[[i]] = getResponse(r$pred, full.matrix = TRUE)
      colnames(probs[[i]]) = task$task.desc$class.levels
    }
    # also fit all base models again on the complete original data set
    base.models[[i]] = train(bl, task)
  }
  names(probs) = names(bls)

  # add true target column IN CORRECT ORDER
  tn = getTaskTargetNames(task)
  test.inds = unlist(rin$test.inds)

  # now start the hill climbing
  probs = lapply(probs, function(x) x[order(test.inds), , drop = FALSE])
  probs[[tn]] = getTaskTargets(task)[test.inds]
  probs[[tn]] = probs[[tn]][order(test.inds)]
  # probs = probs[order(test.inds), , drop = FALSE]
  m = length(bls)
  weights = rep(0, m)
  flag = TRUE
  for (bagind in 1:bagtime) {
    # bagging of models
    bagsize = ceiling(m * bagprob)
    bagmodel = sample(1:m, bagsize)
    weight = rep(0, bagsize)

    # Initial selection of strongest learners
    inds = NULL
    if (init > 0) {
      score = rep(Inf, bagsize)
      for (i in bagmodel) {
        score[i] = metric(probs[[i]], probs[[tn]])
      }
      inds = order(score)[1:init]
      weight[inds] = 1
    }

    selection.size = init
    selection.ind = inds
    # current.prob = rep(0, nrow(probs))
    current.prob = matrix(0, nrow(probs[[1]]), ncol(probs[[1]]))
    old.score = Inf
    if (selection.size > 0) {
      current.prob = Reduce("+", probs[selection.ind])
      old.score = metric(current.prob / selection.size, probs[[tn]])
    }
    flag = TRUE

    while (flag) {
      score = rep(Inf, bagsize)
      for (i in bagmodel) {
        score[i] = metric((probs[[i]] + current.prob) / (selection.size + 1), probs[[tn]])
      }
      inds = order(score)
      if (!replace) {
        ind = setdiff(inds, selection.ind)[1]
      } else {
        ind = inds[1]
      }

      new.score = score[ind]
      if (old.score - new.score < 1e-8) {
        flag = FALSE
      } else {
        current.prob = current.prob + probs[[ind]]
        weights[ind] = weights[ind] + 1
        selection.ind = c(selection.ind, ind)
        selection.size = selection.size + 1
        old.score = new.score
      }
    }
    weights[bagmodel] = weights[bagmodel] + weight
  }
  weights = weights / sum(weights)

  list(method = "hill.climb", base.models = base.models, super.model = NULL,
       pred.train = probs, weights = weights)
}

compressBaseLearners = function(learner, task, parset = list()) {
  lrn = learner
  lrn$method = "hill.climb"
  ensemble.model = train(lrn, task)

  data = getTaskData(task, target.extra = TRUE)
  data = data[[1]]

  pseudo.data = do.call(getPseudoData, c(list(data), parset))
  pseudo.target = predict(ensemble.model, newdata = pseudo.data)
  pseudo.data = data.frame(pseudo.data, target = pseudo.target$data$response)

  td = ensemble.model$task.desc
  type = checkStackSupport(td)

  if (type == "regr") {
    new.task = makeRegrTask(data = pseudo.data, target = "target")
    if (is.null(learner$super.learner)) {
      m = makeLearner("regr.nnet", predict.type = )  # nolint
    } else {
      m = learner$super.learner
    }
  } else {
    new.task = makeClassifTask(data = pseudo.data, target = "target")
    if (is.null(learner$super.learner)) {
      m = makeLearner("classif.nnet", predict.type = "")
    } else {
      m = learner$super.learner
    }
  }

  super.model = train(m, new.task)

  list(method = "compress", base.learners = lrn$base.learners, super.model = super.model,
       pred.train = pseudo.data)
}

### other helpers ###

# Returns response for correct usage in stackNoCV and stackCV and for predictions
getResponse = function(pred, full.matrix = TRUE) {
  # if classification with probabilities
  if (pred$predict.type == "prob") {
    if (full.matrix) {
      # return matrix of probabilities
      td = pred$task.desc
      pred.return = pred$data[, stri_paste("prob", td$class.levels, sep = ".")]
      colnames(pred.return) = td$class.levels
      return(pred.return)
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

# Count the ratio
rowiseRatio = function(probs, levels, model.weight = NULL) {
  m = length(levels)
  p = ncol(probs)
  if (is.null(model.weight)) {
    model.weight = rep(1 / p, p)
  }
  mat = matrix(0, nrow(probs), m)
  for (i in 1:m) {
    ids = matrix(probs == levels[i], nrow(probs), p)
    for (j in 1:p)
      ids[, j] = ids[, j] * model.weight[j]
    mat[, i] = rowSums(ids)
  }
  colnames(mat) = levels
  return(mat)
}

getPseudoData = function(.data, k = 3, prob = 0.1, s = NULL, ...) {
  res = NULL
  n = nrow(.data)
  ori.names = names(.data)
  feat.class = sapply(.data, class)
  ind2 = which(feat.class == "factor")
  ind1 = setdiff(seq_len(ncol(.data)), ind2)
  if (length(ind2) > 0)
    ori.labels = lapply(.data[[ind2]], levels)
  .data = lapply(.data, as.numeric)
  .data = as.data.frame(.data)
  # Normalization
  mn = rep(0, ncol(.data))
  mx = rep(0, ncol(.data))
  for (i in ind1) {
    mn[i] = min(.data[, i])
    mx[i] = max(.data[, i])
    .data[, i] = (.data[, i] - mn[i]) / (mx[i] - mn[i])
  }
  if (is.null(s)) {
    s = rep(0, ncol(.data))
    for (i in ind1) {
      s[i] = sd(.data[, i])
    }
  }
  testNumeric(s, len = ncol(.data), lower = 0)

  # Func to calc dist
  hamming = function(mat) {
    n = nrow(mat)
    m = ncol(mat)
    res = matrix(0, n, n)
    for (j in 1:m) {
      unq = unique(mat[, j])
      p = length(unq)
      for (i in 1:p) {
        ind = which(mat[, j] == unq[i])
        res[ind, -ind] = res[ind, -ind] + 1
      }
    }
    return(res)
  }

  one.nn = function(mat, ind1, ind2) {
    n = nrow(mat)
    dist.mat.1 = matrix(0, n, n)
    dist.mat.2 = matrix(0, n, n)
    if (length(ind1) > 0) {
      dist.mat.1 = as.matrix(stats::dist(mat[, ind1, drop = FALSE]))
    }
    if (length(ind2) > 0) {
      dist.mat.2 = hamming(mat[, ind2, drop = FALSE])
    }
    dist.mat = dist.mat.1 + dist.mat.2
    neighbour = max.col(-dist.mat - diag(Inf, n))
    return(neighbour)
  }

  # Get the neighbour
  neighbour = one.nn(.data, ind1, ind2)

  # Start the loop
  p = ncol(.data)
  for (loop in 1:k) {
    data = .data
    prob.mat = matrix(sample(c(0, 1), n * p, replace = TRUE, prob = c(prob, 1 - prob)), n, p)
    prob.mat = prob.mat == 0
    for (i in 1:n) {
      e = as.numeric(data[i, ])
      ee = as.numeric(data[neighbour[i], ])

      # continuous
      for (j in ind1) {
        if (prob.mat[i, j]) {
          current.sd = abs(e[j] - ee[j]) / s[j]
          tmp1 = rnorm(1, ee[j], current.sd)
          tmp2 = rnorm(1, e[j], current.sd)
          e[j] = tmp1
          ee[j] = tmp2
        }
      }
      for (j in ind2) {
        if (prob.mat[i, j]) {
          tmp = e[j]
          e[j] = ee[j]
          ee[j] = tmp
        }
      }

      data[i, ] = ee
      data[neighbour[i], ] = e
    }
    res = rbind(res, data)
  }
  for (i in ind1)
    res[, i] = res[, i] * (mx[i] - mn[i]) + mn[i]
  res = data.frame(res)
  names(res) = ori.names
  for (i in ind2)
    res[[i]] = factor(res[[i]], labels = ori.labels[[i]])
  return(res)
}

# FIXMEs:
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

# check the learner type to see if it is supported
checkStackSupport = function(td) {
  if (td$type == "regr") {
    type = "regr"
  } else if (td$type == "fcregr") {
    type = "fcregr"
  } else if (td$type == "mfcregr") {
    type = "mfcregr"
  } else if (length(td$class.levels) == 2L) {
    type = "classif"
  } else if (length(td$class.levels) > 2L) {
    type = "multiclassif"
  } else {
    stop(catf("Learners of type %s are not supported", td$type))
  }
  type
}
