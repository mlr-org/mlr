#fixme use learnerparam or ordinary params?

#' Fuse learner with preprocessing.
#'
#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on data / a task, the preprocessing is always performed automatically.
#'
#' @param learner [\code{\link{Learner}}]\cr 
#'   The learner.
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
#'   Named list of default values for params in \code{args} repectively \code{par.set}.
#'   Default is empty list.
#' @return [\code{\link{Learner}}].
#' @export
makePreprocWrapper = function(learner, train, predict, par.set=makeParamSet(), par.vals=list()) {
  checkArg(learner, "Learner")
  checkArg(train, formals=c("data", "target", "args"))
  checkArg(predict, formals=c("data", "target", "args", "control"))
  checkArg(par.set, "ParamSet")
  checkArg(par.vals, "list")
  if (!isProperlyNamed(par.vals))
    stop("'par.vals' must be a properly named list!")
  
  id = paste(learner$id, "preproc", sep=".")
  x = makeBaseWrapper(id, next.learner=learner, par.set=par.set, par.vals=par.vals, cl="PreprocWrapper")
  x$train = train
  x$predict = predict
  return(x)
}

#' @S3method trainLearner PreprocWrapper
trainLearner.PreprocWrapper = function(.learner, .task, .subset, ...) {
  pvs = .learner$par.vals
  pp = .learner$train(data=getTaskData(.task, .subset),
    target=.task$task.desc$target, args=pvs)
  if (!(is.list(pp) && length(pp)==2 && all(names(pp) == c("data", "control")) && 
    is.data.frame(pp$data) && is.list(pp$control)))
    stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
  .task = changeData(.task, pp$data)
  # we have already subsetted!
  m = train(.learner$next.learner, .task)
  #fixme: time and can we do this better?
  # we dont really kow which subset was used after preprocessing and features will have changed
  x = makeChainModel(next.model=m, cl = "PreprocModel")
  x$control = pp$control
  return(x)  
}


#' @S3method predictLearner PreprocWrapper
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  .newdata = .learner$predict(.newdata, .model$task.desc$target, 
    .learner$par.vals, .model$learner.model$control)
  if (!is.data.frame(.newdata))
    stop("Preprocessing must result in a data.frame!")
  NextMethod(.newdata=.newdata)
}


