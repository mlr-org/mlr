#' @title Wraps a classifier for cost-sensitive learning to produce a weighted pairs model.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#' Models can easily be accessed via [getLearnerModel].
#'
#' For each pair of labels, we fit a binary classifier.
#' For each observation we define the label to be the element of the pair with minimal costs.
#' During fitting, we also weight the observation with the absolute difference in costs.
#' Prediction is performed by simple voting.
#'
#' This approach is sometimes called cost-sensitive one-vs-one (CS-OVO),
#' because it is obviously very similar to the
#' one-vs-one approach where one reduces a normal multi-class problem to
#' multiple binary ones and aggregates by voting.
#'
#' @template arg_learner_classif
#' @return ([Learner]).
#' @export
#' @references
#' Lin, HT.:
#' Reduction from Cost-sensitive Multiclass Classification to
#' One-versus-one Binary Classification.
#' In: Proceedings of the Sixth Asian Conference on Machine Learning.
#' JMLR Workshop and Conference Proceedings, vol 39, pp. 371-386. JMLR W&CP (2014).
#' <http://www.jmlr.org/proceedings/papers/v39/lin14.pdf>
#' @family costsens
#' @aliases CostSensWeightedPairsWrapper CostSensWeightedPairsModel
makeCostSensWeightedPairsWrapper = function(learner) {
  learner = checkLearner(learner, "classif", props = "weights")
  learner = setPredictType(learner, "response")
  id = stri_paste("costsens", learner$id, sep = ".")
  makeHomogeneousEnsemble(id, "costsens", learner, package = learner$package,
    learner.subclass = "CostSensWeightedPairsWrapper", model.subclass = "CostSensWeightedPairsModel")
}

#' @export
trainLearner.CostSensWeightedPairsWrapper = function(.learner, .task, .subset = NULL, ...) {

  # note that no hyperpars can be in ..., they would refer to the wrapper
  .task = subsetTask(.task, subset = .subset)
  costs = getTaskCosts(.task)
  td = getTaskDesc(.task)
  classes = td$class.levels
  k = length(classes)
  feats = getTaskData(.task)
  models = vector("list", length = k * (k - 1) / 2)

  counter = 1
  for (i in 1:(k - 1)) {
    a1 = classes[i]
    for (j in (i + 1):k) {
      a2 = classes[j]
      y = ifelse(costs[, a1] < costs[, a2], a1, a2)
      # if on the sample one alg is always better, always predict it
      if (all(y == a1) || all(y == a2)) {
        models[[counter]] = y[1]
      } else {
        feats$..y.. = y
        task = makeClassifTask(data = feats, target = "..y..",
          check.data = FALSE, fixup.data = "quiet")
        w = abs(costs[, a1] - costs[, a2])
        models[[counter]] = train(.learner$next.learner, task, weights = w)
      }
      counter = counter + 1L
    }
  }
  makeHomChainModel(.learner, models)
}


#' @export
predictLearner.CostSensWeightedPairsWrapper = function(.learner, .model, .newdata, ...) {
  classes = .model$task.desc$class.levels
  preds = predictHomogeneousEnsemble(.learner, .model, .newdata, ...)
  factor(apply(preds, 1L, computeMode), levels = classes)
}

#' @export
getLearnerProperties.CostSensWeightedPairsWrapper = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), c("weights", "prob"))
}
