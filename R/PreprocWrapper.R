#FIXME: use learnerparam or ordinary params?

#' Fuse learner with preprocessing.
#'
#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested.
#' If the train or predict function is called on data / a task, the preprocessing is always performed automatically.
#'
#' @template arg_learner
#' @param train [\code{function(data, target, args)}]\cr
#'   Function to preprocess the data before training.
#'   \code{target} is a string and denotes the target variable in \code{data}.
#'   \code{args} is a list of further arguments and parameters to influence the
#'   preprocessing.
#'   Must return a \code{list(data, control)}, where \code{data} is the preprocessed
#'   data and \code{control} stores all information necessary to do the preprocessing
#'   before predictions.
#' @param predict [\code{function(data, target, args, control)}]\cr
#'   Function to preprocess the data before prediction.
#'   \code{target} is a string and denotes the target variable in \code{data}.
#'   \code{args} are the args that were passed to \code{train}.
#'   \code{control} is the object you returned in \code{train}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set of \code{\link[ParamHelpers]{LearnerParam}} objects to describe the
#'   parameters in \code{args}.
#'   Default is empty set.
#' @param par.vals [\code{list}]\cr
#'   Named list of default values for params in \code{args} respectively \code{par.set}.
#'   Default is empty list.
#' @return [\code{\link{Learner}}].
#' @family wrapper
#' @export
makePreprocWrapper = function(learner, train, predict, par.set = makeParamSet(), par.vals = list()) {
  learner = checkLearner(learner)
  assertFunction(train, args = c("data", "target", "args"))
  assertFunction(predict, args = c("data", "target", "args", "control"))
  assertClass(par.set, classes = "ParamSet")
  checkList(par.vals)
  if (!isProperlyNamed(par.vals))
    stop("'par.vals' must be a properly named list!")

  id = paste(learner$id, "preproc", sep = ".")
  x = makeBaseWrapper(id, type = learner$type, next.learner = learner, par.set = par.set,
    par.vals = par.vals, learner.subclass = "PreprocWrapper", model.subclass = "PreprocModel")
  x$train = train
  x$predict = predict
  return(x)
}

#' @export
trainLearner.PreprocWrapper = function(.learner, .task, .subset, ...) {
  pvs = .learner$par.vals
  pp = .learner$train(data = getTaskData(.task, .subset),
    target = getTaskTargetNames(.task), args = pvs)
  # FIXME: why is the order important?
  if (!(is.list(pp) && length(pp) == 2L && all(names(pp) == c("data", "control")) &&
    is.data.frame(pp$data) && is.list(pp$control)))
    stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
  .task = changeData(.task, pp$data)
  # we have already subsetted!
  m = train(.learner$next.learner, .task)
  # FIXME: time and can we do this better?
  # we dont really kow which subset was used after preprocessing and features will have changed
  x = makeChainModel(next.model = m, cl = "PreprocModel")
  x$control = pp$control
  return(x)
}


#' @export
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  .newdata = .learner$predict(.newdata, .model$task.desc$target,
    .learner$par.vals, .model$learner.model$control)
  if (!is.data.frame(.newdata))
    stop("Preprocessing must result in a data.frame!")
  NextMethod(.newdata = .newdata)
}
