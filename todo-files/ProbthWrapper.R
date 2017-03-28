setClass(
  "ProbthWrapper",
  contains = "BaseWrapper"
)


#' Fuses a classifier with thresholding. Creates a learner object, which can be
#' used like any other learner object, but which produces discrete class labels 
#' from probailities of teh base learner according to thresholds.
#' These thresholds are additional hyperparameters of the new learner and 
#' can therefore be tuned. 
#' 
#' See \code{\link{setThreshold}} for details of thresholding.  
#'
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param classes [character] \cr
#'   Classes of future classification task.
#' 
#' @return \code{\linkS4class{Learner}}.
#' 
#' @title Fuse learner with probability thresholding.
#' @export

makeProbthWrapper = function(learner, classes) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (learner@properties[["type"]] != "classif")
    stop("Only classifiers can be used as base learners!")
  if (learner@predict.type != "prob")
    stop("The predict.type of the base learner must be 'prob'!")
  a = as.list(rep(0.5, length(classes)))
  names(a) = paste("probth", classes, sep=".")
  ps = do.call(makeParamSet, 
    lapply(names(a), function(x) makeNumericLearnerParam(id=x, lower=0, upper=1)))
  w = new("ProbthWrapper", learner=learner, par.set=ps, par.vals=a)
  w@properties["prob"] = FALSE
  setPredictType(w, "response")
}

#' @rdname predictLearner
setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "ProbthWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame"
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    p = predictLearner(.learner@learner, .model, .newdata)
    ths = unlist(.learner@par.vals)
    # remove "probth"    
    names(ths) = sapply(strsplit(names(ths), "\\."), function(x) x[2])
    setThreshold(p, threshold=ths)
  }
) 
