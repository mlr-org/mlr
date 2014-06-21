#' @title Fuse learner with the bagging technique and oversampling for imbalancy correction.
#'
#' Fuses a learner with the bagging method
#' (i.e., similar to what a \code{randomForest} does).
#' Creates a learner object, which can be
#' used like any other learner object.
#' Models can easily be accessed via \code{\link{getBaggingModels}}.
#'
#' OverBagging is implemented as follows:
#' For each iteration a random data subset is sampled. Minority class examples
#' are oversampled and majority class examples are sampled with replacement. 
#' Note that this is usually handled in a slightly different way
#' in the random forest where features are sampled at each tree split).
#'
#' Prediction works as follows:
#' For classification we do majority voting to create a discrete label and
#' probabilities are predicted by considering the proportions of all predicted labels.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner. Prediction type of the basic learner must be \dQuote{response}.
#' @param obw.iters [\code{integer(1)}]\cr
#'   Iterations = number of fitted models in bagging.
#'   Default is 10.
#' @param obw.rate [\code{numeric(1)}]\cr
#'   Factor to upsample the smaller class in each bag.
#'   Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} (= labels) or \dQuote{prob}
#'   (= probabilities and labels by selecting the ones with maximal probability).
#'   Regression: \dQuote{response} (= mean response) or \dQuote{se} (= standard errors
#'   and mean response).
#'   Default is \dQuote{response}.
#' @return [\code{\link{Learner}}].
#' @export
makeOverBaggingWrapper = function(learner, obw.iters = 10L, obw.rate = 2,
  obw.maxcl = "boot", predict.type = "response") {
  
  learner = checkLearner(learner, "classif")
  obw.iters = convertInteger(obw.iters)
  checkArg(obw.iters, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  checkArg(obw.rate, "numeric", len = 1L, na.ok = FALSE, lower = 1)
  checkArg(obw.maxcl, "character", choices=c("boot","all"))  
  
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be response.")
  id = paste(learner$id, "overbagged", sep = ".")
  packs = learner$packages
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "obw.iters", lower = 1L, default = 10L),
    makeNumericLearnerParam(id = "obw.rate", lower = 1),
    makeDiscreteLearnerParam(id = "obw.maxcl", c("boot","all"))    
  )
  pv = list(obw.iters = obw.iters, obw.rate = obw.rate, obw.maxcl = obw.maxcl)
  x = makeBaseWrapper(id, learner, packs, par.set = ps, par.vals = pv, cl = "OverBaggingWrapper")
  x = addProperties(x, "prob")
   
  if(!is.na(predict.type))
    x = setPredictType(x, predict.type)
  x
}

#' @export
trainLearner.OverBaggingWrapper = function(.learner, .task, .subset, obw.iters,
   obw.rate, obw.maxcl, bag, ...) {

  .task = subsetTask(.task, subset = .subset)
  if(obw.maxcl == "boot") {
    y = getTaskTargets(.task)
    z = getMinMaxClass(y)
    inds1 = z$min.inds # min class
    inds2 = z$max.inds # maj class
    newsize = round(length(inds1) * obw.rate)
  }
    
  models = lapply(seq_len(obw.iters), function(i) { 
    if(obw.maxcl == "boot") {
      # returns indexes of oversampled minority class examples and bootstrapped majority class examples
      bag = c(sample(inds1, newsize, replace = TRUE),
              sample(inds2, length(inds2), replace = TRUE))
    } else {
      # returns indexes of oversampled minority class examples and ALL majority class examples
      bag = sampleBinaryClass(getTaskTargets(.task), obw.rate, cl = "min", replace = TRUE)
    }
    train(.learner$next.learner, .task, subset = bag)
  })
  makeChainModel(next.model = models, cl = "OverBaggingModel")
}

#' @export
predictLearner.OverBaggingWrapper = function(.learner, .model, .newdata, ...) {
  models = getBaggingModels(.model)
  p = sapply(models, function(m) {
    nd = .newdata[, m$features, drop=FALSE]
    as.character(predict(m, newdata=nd, ...)$data$response)
  })
  if (.learner$predict.type == "response") {
    g = as.factor(apply(p, 1L, computeMode))
  } else {
    levs = .model$task.desc$class.levels
    p = apply(p, 1L, function(x) {
    x = factor(x, levels = levs)
    as.numeric(prop.table(table(x)))
    })
    setColNames(t(p), levs)
  }
}

#' @export
makeWrappedModel.OverBaggingWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  class(x) = c("OverBaggingModel", class(x))
  return(x)
}

#' @export
print.OverBaggingModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("OverBagged Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}
