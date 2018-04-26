#' @title Create a stacked learner object.
#'
#' @description A stacked learner uses predictions of several base learners to
#'   obtain level 1 data. This prediction is used to train a super learner,
#'   apply ensemble selection or just combine the methods using averaging. The
#'   following stacking methods are available:
#'
#'   \describe{ \item{`aggregate`}{Averaging of base learner predictions without
#'   weights.} \item{`superlearner`}{Fits a super learner to base learner
#'   predictions. Base learner predictions are computed out-of-sample
#'   predictions from cross-validation (the internal resampling strategy can be
#'   set via the `resampling` argument). See Wolpert, 1992 for details.}
#'   \item{`ensembleselection`}{Select a subset of base learner predictions by
#'   ensenemble selection. See Caruana et al., 2004 for details.} }
#'
#' @param id (`character(1)`)`  Id string for object. Used to display object and
#'   for model saving. Default is "stack".
#' @param method (`character(1)`)\cr \describe{ \item{`aggregate`}{Averaging of
#'   base learner predictions without weights.} \item{`superlearner`}{Building a
#'   super learner using crossvalidated predictions of the base learners.}
#'   \item{`ensembleselection`}{For averaging the (cross validated) predictions
#'   of the base learners, with the weights learned from ensemble selection
#'   algorithm.} }
#' @param base.learners (list of [Learner] | `character(1)`)\cr A list of learners created with
#'   ([makeLearner]). The prediction type can be changed using `setPredictType`.
#' @param super.learner learner ([Learner] | `character(1)`)\cr The super
#'   learner that makes the final prediction based on the cross validated base
#'   learners predictions. If you pass a string, the super learner will be
#'   created via `makeLearner`. Only used for `method = 'superlearner'`. Default
#'   is `NULL`.
#' @param predict.type (`character(1)`)\cr Sets the type of the final
#'   prediction. For method `super.learner` the predict type can also be set
#'   within `super.learner`. If the type of the base learner prediction, which
#'   is set up within `base.learners`, is \describe{ \item{`prob`}{`predict.type
#'   = 'prob'` will use the aggregate of all base learner predictions and
#'   `predict.type = 'response'` will use the class with highest probability as
#'   final prediction.} \item{`response`}{For classification tasks with
#'   `predict.type = 'prob'`, the final prediction will be the relative
#'   frequency based on the predicted base learner classes and classification
#'   tasks with `predict.type = 'response'` will use majority vote of the base
#'   learner predictions to determine the final prediction. For regression
#'   tasks, the final prediction will be the aggregate of the base learner
#'   predictions.} }
#' @param resampling ([ResampleDesc])\cr Resampling strategy for `method =
#'   'superlearner'` and `method = 'ensembleselection'`. Only CV is allowed for
#'   resampling. The default `NULL` uses 5-fold CV.
#' @param use.feat (`logical(1)`)\cr Whether the original features should also
#'   be passed to the super learner. Only used for `method = 'superlearner'`.
#'   Default is `FALSE`.
#' @param es.par.vals (`list`) the parameters for `ensembleselection` method.
#'   The following parameter values can be set:\cr \describe{
#'   \item{`replace`}{Can a base learner be selected more than once?}
#'   \item{`init`}{Number of best models being included before the selection
#'   algorithm.} \item{`bagprob`}{The proportion of models being considered in
#'   one round of selection.} \item{`bagtime`}{The number of rounds of the
#'   bagging selection.} \item{`metric`}{The result evaluation metric function
#'   taking two parameters `pred` and `true`, the smaller the score the better.}
#'   \item{`tolerance`}{Minimum improvement in ensemble performance in order to
#'   continue adding learners.} }
#' @param save.on.disc (`logical(1)`)\cr If set to `TRUE`, base models are saved
#'   on disc into the working directory. This setting saves memory when huge
#'   models are fitted but also might take longer. Later, during prediction this
#'   models are loaded. Models are saved with the name
#'   _saved.models<stack.id>.<base.learner.id>.RData_. Note that this only works
#'   for train-predict procedures as well as for resampling using holdout.
#'   Applying outer cross validation will result in wrong predictions due to the
#'   fact that model names does not seperate between different resample
#'   iterations. Default is `FALSE`.
#' @param save.preds (`logical(1)`)\cr
#'   If set to `FALSE` models will not contain
#'   predictions. This reduces the object size. Note that function
#'   `resampleStackedLearnerAgain` does not work if saving prediction is
#'   disabled. Default is `TRUE`.
#' @references Wolpert, David H. "Stacked generalization." Neural networks 5.2
#'   (1992): 241-259.
#'   \url{http://www.machine-learning.martinsewell.com/ensembles/stacking/Wolpert1992.pdf}
#'
#' @references Caruana, Rich, et al. "Ensemble selection from libraries of
#'   models." Proceedings of the twenty-first international conference on
#'   Machine learning. ACM, 2004.
#'   \url{http://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
#'
#' @examples
#' \dontrun{
#'   # Classification
#'   data(iris)
#'   tsk = makeClassifTask(data = iris, target = "Species")
#'   base = c("classif.rpart", "classif.lda", "classif.svm")
#'   lrns = lapply(base, makeLearner)
#'   lrns = lapply(lrns, setPredictType, "prob")
#'   stk = makeStackedLearner(method = "ensembleselection", base.learners = lrns,
#'     predict.type = "prob", es.par.vals = list(init = 1, metric = mmce))
#'   res = resample(stk, tsk, cv5, mmce)
#'
#'   # Regression
#'   data(BostonHousing, package = "mlbench")
#'   tsk = makeRegrTask(data = BostonHousing, target = "medv")
#'   base = c("regr.rpart", "regr.svm")
#'   lrns = lapply(base, makeLearner)
#'   stk = makeStackedLearner(base.learners = lrns, predict.type = "response",
#'     method = "ensembleselection", es.par.vals = list(init = 1, metric = mae))
#'   m = train(stk, tsk)
#'   res = predict(m, tsk)
#' }
#' @export

makeStackedLearner = function(id = "stack", method = "superlearner", base.learners,
  predict.type = NULL, resampling = NULL, super.learner = NULL, use.feat = FALSE,
  es.par.vals = list(), save.on.disc = FALSE, save.preds = TRUE) {
  # checking
  base.learners = lapply(base.learners, checkLearner)
  if (!is.null(super.learner)) {
    super.learner = checkLearner(super.learner)
    if (!is.null(predict.type)) super.learner = setPredictType(super.learner, predict.type)
  }

  baseType = unique(extractSubList(base.learners, "type"))
  assertChoice(method, c("aggregate", "superlearner", "ensembleselection"))
  assertCharacter(id, min.chars = 1)
  assertFlag(save.on.disc)
  assertFlag(save.preds)

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
  if ("se" %in% bm.pt | (!is.null(predict.type) && predict.type == "se") |
       (!is.null(super.learner) && super.learner$predict.type == "se"))
   stop("Predicting standard errors currently not supported.")
  if (length(bm.pt) > 1L)
    stop("Base learner must all have the same predict type!")
  if ((method %in% c("aggregate", "ensembleselection")) & (!is.null(super.learner) | is.null(predict.type)) )

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
  lrn$es.par.vals = es.par.vals
  lrn$fix.factors.prediction = TRUE
  lrn$save.on.disc = save.on.disc
  lrn$save.preds = save.preds

  return(lrn)
}

# FIXME: Saving models does not work for outer resampling?
