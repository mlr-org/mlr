#' Fuses a base learner with a search strategy to select variables. Creates a learner object, which can be
#' used like any other learner object, but which internally uses varsel. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of variables. Finally, a model is fitted on the complete training data with these variables and returned.    
#' See \code{\link{varsel}} for more details.
#' 
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param bit.names [character]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.    
#' @param bits.to.features [function(x, task)]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features. 
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.      
#' @param control [\code{\linkS4class{VarselControl}}] 
#'   Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param log.fun [function(learner, task, resampling, measure, par.set, control, opt.path, x, y)]\cr
#'   Called after every hyperparameter evaluation. Default is to print performance via mlr logger. 
#' 
#' @return \code{\linkS4class{Learner}}.
#' @export
#' @seealso \code{\link{varsel}}, \code{\link{VarselControl}} 
#' @title Fuse learner with variable selection.

makeVarselWrapper = function(learner, resampling, measures, bit.names, bits.to.features, control, log.fun) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (missing(measures))
    measures = mlr:::default.measures(learner)
  if (is(measures, "Measure"))
    measures = list(measures)   
  if (missing(bit.names))
    bit.names = character(0)
  if (missing(bits.to.features))
    bits.to.features = function(x, task) binary.to.vars(x, getFeatureNames(task)) 
  if (missing(log.fun))
    log.fun = mlrVarsel:::log.fun.varsel
  mlrVarsel:::checkVarselParset(learner, par.set, bit.names, control) 
  makeOptWrapper(learner, resampling, measures, makeParamSet(), bit.names, bits.to.features,
    control, log.fun)
}

trainLearner.VarselWrapper = function(.learner, .task, .subset,  ...) {
  # fixme: strange error if we remove :::? maybe rename subset...
  task = subsetData(.task, .subset)
  if (length(.learner$bit.names) == 0) 
    or = varsel(.learner$learner, task, .learner$resampling, .learner$control,
                .learner$measures)
  else  
    or = varsel(.learner$learner, task, .learner$resampling, .learner$control,
                .learner$measures, .learner$bit.names, .learner$bits.to.features)
  task = subsetData(task, vars=or$x)
  m = train(.learner$learner, task)
  # set the opt result as attribute, so we can extract it later 
  attr(m, "opt.result") = or
  return(m)
}
