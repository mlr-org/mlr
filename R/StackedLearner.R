#' @title Create a stacked learner object.
#'
#' @description A stacked learner uses predictions of several base learners to 
#' obtain level 1 data. This prediction is used to train a super learner, 
#' apply ensemble selection or just combine the methods using averaging.
#' The following stacking methods are available:
#'
#'  \describe{
#'   \item{\code{aggregate}}{Averaging of base learner predictions without weights.}
#'   \item{\code{superlearner}}{Fits the super learner, where the base learner predictions are computed
#'   by crossvalidated predictions (the internal resampling strategy can be set via the \code{resampling} argument).}
#'   \item{\code{ensembleselection}}{Select a subset of base learner predictions by ensenemble selection}
#'  }
#'
#' @param id [\code{character(1)}]  Id string for object. Used to display object 
#' and for model saving. Default is "stack".
#' @param method [\code{character(1)}]\cr
#'   \dQuote{aggregate} for averaging the predictions of the base learners,
#'   \dQuote{superlearner} for building a super learner using crossvalidated predictions of the base learners.
#'   \dQuote{ensembleselection} for averaging the (cross validated) predictions of the base learners, with the weights learned from
#'   ensemble selection algorithm.
#' @param base.learners [(list of) \code{\link{Learner}}]\cr
#'   A list of learners created with \code{makeLearner}. The prediction type can
#'   be changed using \code{setPredictType}.
#' @param predict.type [\code{character(1)}]\cr
#'   Sets the type of the final prediction.
#'   For method \code{super.learner} the predict type can also be set within \code{super.learner}.
#'   If the type of the base learner prediction, which is set up within \code{base.learners}, is
#'   \describe{
#'    \item{\code{"prob"}}{then \code{predict.type = 'prob'} will use the aggregate of all
#'    base learner predictions and \code{predict.type = 'response'} will use
#'    the class with highest probability as final prediction.}
#'    \item{\code{"response"}}{then, for classification tasks with \code{predict.type = 'prob'},
#'    the final prediction will be the relative frequency based on the predicted base learner classes
#'    and classification tasks with \code{predict.type = 'response'} will use majority vote of the base
#'    learner predictions to determine the final prediction.
#'    For regression tasks, the final prediction will be the aggregate of the base learner predictions.}
#'   }
#' @param resampling [\code{\link{ResampleDesc}}]\cr
#'   Resampling strategy for \code{method = 'superlearner'} and \code{method = 'ensembleselection'}.
#'   Only CV is allowed for resampling. The default \code{NULL} uses 5-fold CV.
#' @param super.learner [\code{\link{Learner} | character(1)}]\cr
#'   The super learner that makes the final prediction based on the cross validated
#'   base learners predictions. If you pass a string, the super learner will be 
#'   created via \code{makeLearner}. Only used for \code{method = 'superlearner'}. 
#'   Default is \code{NULL}.
#' @param use.feat [\code{logical(1)}]\cr
#'   Whether the original features should also be passed to the super learner.
#'   Only used for \code{method = 'superlearner'}. Default is \code{FALSE}.
#' @param parset the parameters for \code{ensembleselection} method, including
#' \describe{
#'   \item{\code{replace}}{Whether a base learner can be selected more than once within a bagging iteration.}
#'   \item{\code{init}}{Number of best models being included at the beginning of each bagging iteration.}
#'   \item{\code{bagprob}}{The proportion of models being considered in one bagging iteration.}
#'   \item{\code{bagtime}}{The number of the bagging iterations.}
#'   \item{\code{metric}}{The evaluation metric. Must be an object of type \code{Measure}.}
#'   \item{\code{tolerance}}{The tolerance when inner loop should stop.}
#' }
#' @param save.on.disc [\code{logical(1)}]\cr 
#'   If set to \code{TRUE}, base models are saved on disc at the working directory. 
#'   This setting saves memory when huge models are fitted but also might take longer. 
#'   Later during prediction this models are loaded. Models are saved with the 
#'   name "saved.models<stack.id>.<base.learner.id>.RData".
#'   Note that it only works for train-predict procedures as well as for resampling using holdout. 
#'   Applying outer cross validation will result in wrong predictions due to the 
#'   fact that model names does not seperate between different resample iterations.
#'   Default is \code{FALSE}
#' @param save.preds [\code{logical(1)}]\cr 
#'   If set to \code{FALSE} models will not contain predictions. This reduce the 
#'   object size. Note that function \code{resampleStackedLearnerAgain} does not 
#'   work if saving prediction is disabled. Default is \code{TRUE}.
#' @references Wolpert, David H. "Stacked generalization." Neural networks 5.2 (1992): 241-259.
#'   \url{http://www.machine-learning.martinsewell.com/ensembles/stacking/Wolpert1992.pdf}
#' @references Caruana, Rich, et al. "Ensemble selection from libraries of models." 
#'   Proceedings of the twenty-first international conference on Machine learning. 
#'   ACM, 2004. \url{http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
#' @examples
#' \dontrun{
#'   # Classification
#'   data(iris)
#'   tsk = makeClassifTask(data = iris, target = "Species")
#'   base = c("classif.rpart", "classif.lda", "classif.svm")
#'   lrns = lapply(base, makeLearner)
#'   lrns = lapply(lrns, setPredictType, "prob")
#'   stk = makeStackedLearner(method = "ensembleselection", base.learners = lrns, predict.type = "prob", parset = list(init = 1, metric = mmce))
#'   res = resample(stk, tsk, cv5, mmce)
#'
#'   # Regression
#'   data(BostonHousing, package = "mlbench")
#'   tsk = makeRegrTask(data = BostonHousing, target = "medv")
#'   base = c("regr.rpart", "regr.svm")
#'   lrns = lapply(base, makeLearner)
#'   stk = makeStackedLearner(base.learners = lrns, predict.type = "response", method = "ensembleselection", parset = list(init = 1, metric = mae))
#'   m = train(stk, tsk)
#'   res = predict(m, tsk)
#' }
#' @export

makeStackedLearner = function(id = "stack", method = "superlearner", base.learners, 
  predict.type = NULL, resampling = NULL, super.learner = NULL, use.feat = FALSE, 
  parset = list(), save.on.disc = FALSE, save.preds = TRUE) {
  # checking
  if (is.character(base.learners)) base.learners = lapply(base.learners, checkLearner)
  if (!is.null(super.learner)) {
    super.learner = checkLearner(super.learner)
    if (!is.null(predict.type)) super.learner = setPredictType(super.learner, predict.type)
  }

  baseType = unique(extractSubList(base.learners, "type"))
  assertChoice(method, c("aggregate", "superlearner", "ensembleselection"))
  assertCharacter(id, min.chars = 1)
  assertLogical(save.on.disc, len = 1)
  assertLogical(save.preds, len = 1)
  
  if (method %in% c("superlearner", "ensembleselection")) {
    if (is.null(resampling)) {
      resampling = makeResampleDesc("CV", iters = 5L, stratify = ifelse(baseType == "classif", TRUE, FALSE))
    } else {
      assertClass(resampling, "CVDesc")
    }
  } else {
    assertClass(resampling, "NULL")
  }

  bm.pt = unique(extractSubList(base.learners, "predict.type"))
  #if ("se" %in% bm.pt | (!is.null(predict.type) && predict.type == "se") |
  #      (!is.null(super.learner) && super.learner$predict.type == "se"))
  #  stop("Predicting standard errors currently not supported.")
  if (length(bm.pt) > 1L)
    stop("Base learner must all have the same predict type!")
  if ((method %in% c("aggregate", "ensembleselection")) & (!is.null(super.learner) | is.null(predict.type)) )
    stop("No super learner needed for this method or the 'predict.type' is not specified.")
  if (method %in% "superlearner" & is.null(super.learner))
    stop("You have to specify a super learner for this method.")
  #if (method != "aggregate" & !is.null(predict.type))
  #  stop("Predict type has to be specified within the super learner.")
  if ((method %in% c("aggregate", "ensembleselection")) & use.feat)
    stop("The original features cannot be used for this method")
  #if (!inherits(resampling, "CVDesc")) # new 
  #  stop("Currently only CV is allowed for resampling!") # new
  # lrn$predict.type is "response" by default change it using setPredictType
  lrn =  makeBaseEnsemble(
    id = id,
    base.learners = base.learners,
    cl = "StackedLearner"
  )
  if (!is.null(super.learner)) {
    lrn = setPredictType(lrn, predict.type = super.learner$predict.type)
  } else {
    lrn = setPredictType(lrn, predict.type = predict.type)
  }


  lrn$short.name = "stack"
  lrn$name = "StackedLearner"
  lrn$method = method
  lrn$resampling = resampling
  lrn$super.learner = super.learner
  lrn$use.feat = use.feat
  lrn$parset = parset
  lrn$fix.factors.prediction = TRUE
  lrn$save.on.disc = save.on.disc
  lrn$save.preds = save.preds
  return(lrn)
}

