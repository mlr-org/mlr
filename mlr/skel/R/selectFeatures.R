#' Feature selection by wrapper approach.
#'
#' Optimizes the features for a classification or regression problem by choosing a variable selection wrapper approach.
#' Allows for different optimization methods, such as forward search or a genetic algorithm.
#' You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at the 
#' subclasses of [\code{\link{FeatSelControl}}].
#'
#' All algorithms operate on a 0-1-bit encoding of candidate solutions. Per default a single bit corresponds
#' to a single feature, but you are able to change this by using the arguments \code{bit.names} 
#' and \code{bits.to.features}. Thus allowing you to switch on whole groups of features with a single bit.  
#' 
#' @param learner [\code{\link[mlr]{Learner}}]\cr 
#'   The learner.
#' @param task [\code{\link[mlr]{SupervisedTask}}]\cr
#'   The task.
#' @param resampling [\code{\link[mlr]{ResampleInstance}} | \code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to feature sets. If you pass a description, 
#'   it is instantiated once at the beginning by default, so all points are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at \code{\link{FeatSelControl}}.   
#' @param control [see \code{\link{FeatSelControl}}]
#'   Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param measures [list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during selection, others are simply evaluated.  
#' @param bit.names [character]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.    
#' @param bits.to.features [function(x, task)]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features. 
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.      
#' @param show.info [\code{logical(1)}]\cr
#'   A logical value, indicating whether information should be printed. 
#'   The default is TRUE.
#' @return [\code{\link{FeatSelResult}}].
#' @export
#' @examples
#' task <- makeClassifTask(data=iris, target="Species")
#' lrn <- makeLearner("classif.rpart")
#' rdesc <- makeResampleDesc("Holdout")
#' 
#' ## Now create control-objects for each of the possible feature selection algorithms:
#' ctrlSeq <- makeFeatSelControlSequential(method="sfs", maxit=NA)
#' ctrlGA <- makeFeatSelControlGA(maxit=5, max.features=NA, crossover.rate=0.5, mutation.rate=0.1, mu=10, lambda=5)
#' ctrlRand <- makeFeatSelControlRandom(maxit=10, max.features=NA, prob=0.5)
#' ctrlExh <- makeFeatSelControlExhaustive(maxit=NA, max.features=NA)
#' 
#' ## Let's run the feature selction algorithm:
#' 
#' sfSeq <- selectFeatures(lrn, task, rdesc, control=ctrlSeq)
#' sfSeq
#' sfGA <- selectFeatures(lrn, task, rdesc, control=ctrlGA)
#' sfGA
#' sfRand <- selectFeatures(lrn, task, rdesc, control=ctrlRand)
#' sfRand
#' sfExh <- selectFeatures(lrn, task, rdesc, control=ctrlExh)
#' sfExh
selectFeatures = function(learner, task, resampling, control, measures, 
  bit.names, bits.to.features, show.info=TRUE) {
  
  checkArg(learner, "Learner")
  checkArg(task, "SupervisedTask")
  if (!inherits(resampling, "ResampleDesc") &&  !inherits(resampling, "ResampleInstance"))
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
  if (missing(measures))
    measures = default.measures(task)
  if (inherits(measures, "Measure"))
    measures = list(measures)   
  checkListElementClass(measures, "Measure")
  if (missing(bit.names)) {
    bit.names = getTaskFeatureNames(task)
  } else {
    checkArg(bit.names, "character", na.ok=FALSE)
  }
  if (missing(bits.to.features)) {
    bits.to.features = function(x, task) binaryToFeatures(x, getTaskFeatureNames(task)) 
  } else {
    checkArg(bits.to.features, "function", formals=c("x", "task"))
  }
  checkArg(control, "FeatSelControl")
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)

  par.set = lapply(bit.names, function(bn) makeIntegerParam(bn))
  par.set = do.call(makeParamSet, par.set)
  #checkVarselParset(learner, par.set, bit.names, control)
  opt.path = makeOptPathDFFromMeasures(par.set, measures)

  cl = as.character(class(control))[1]
  sel.func = switch(cl,
    FeatSelControlRandom = selectFeaturesRandom,
    FeatSelControlExhaustive = selectFeaturesExhaustive,
    FeatSelControlSequential = selectFeaturesSequential,
    FeatSelControlGA = selectFeaturesGA
  )
  
  sel.func(learner, task, resampling, measures, bit.names, 
    bits.to.features, control, opt.path, show.info)
}
