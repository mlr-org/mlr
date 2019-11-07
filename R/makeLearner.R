#' @title Create learner object.
#'
#' @description For a classification learner the `predict.type` can be set to
#' \dQuote{prob} to predict probabilities and the maximum value selects the
#' label. The threshold used to assign the label can later be changed using the
#' [setThreshold] function.
#'
#' To see all possible properties of a learner, go to: [LearnerProperties].
#'
#' @section `par.vals` vs. `...`:
#'
#'   The former aims at specifying default hyperparameter settings from `mlr`
#'   which differ from the actual defaults in the underlying learner. For
#'   example, `respect.unordered.factors` is set to `order` in `mlr` while the
#'   default in [ranger::ranger] depends on the argument `splitrule`.
#'   `getHyperPars(<learner>)` can be used to query hyperparameter defaults that
#'   differ from the underlying learner. This function also shows all
#'   hyperparameters set by the user during learner creation (if these differ
#'   from the learner defaults).
#'
#' @template arg_lrncl
#' @param id (`character(1)`)\cr Id string for object. Used to display object.
#'   Default is `cl`.
#' @param predict.type (`character(1)`)\cr Classification: \dQuote{response} (=
#'   labels) or \dQuote{prob} (= probabilities and labels by selecting the ones
#'   with maximal probability). Regression: \dQuote{response} (= mean response)
#'   or \dQuote{se} (= standard errors and mean response). Survival:
#'   \dQuote{response} (= some sort of orderable risk) or \dQuote{prob} (= time
#'   dependent probabilities). Clustering: \dQuote{response} (= cluster IDS) or
#'   \dQuote{prob} (= fuzzy cluster membership probabilities), Multilabel:
#'   \dQuote{response} (= logical matrix indicating the predicted class labels)
#'   or \dQuote{prob} (= probabilities and corresponding logical matrix
#'   indicating class labels). Default is \dQuote{response}.
#' @template arg_predictthreshold
#' @param fix.factors.prediction (`logical(1)`)\cr In some cases, problems occur
#'   in underlying learners for factor features during prediction. If the new
#'   features have LESS factor levels than during training (a strict subset),
#'   the learner might produce an  error like \dQuote{type of predictors in new
#'   data do not match that of the training data}. In this case one can repair
#'   this problem by setting this option to `TRUE`. We will simply add the
#'   missing factor levels missing from the test feature (but present in
#'   training) to that feature. Default is `FALSE`.
#' @param ... (any)\cr Optional named (hyper)parameters. If you want to set
#'   specific hyperparameters for a learner during model creation, these should
#'   go here. You can get a list of available hyperparameters using
#'   `getParamSet(<learner>)`. Alternatively hyperparameters can be given using
#'   the `par.vals` argument but `...` should be preferred!
#' @param par.vals ([list])\cr Optional list of named (hyper)parameters. The
#'   arguments in `...` take precedence over values in this list. We strongly
#'   encourage you to use `...` for passing hyperparameters.
#' @param config (named [list])\cr Named list of config option to overwrite
#'   global settings set via [configureMlr] for this specific learner.
#'
#' @section regr.randomForest:
#'
#' For this learner we added additional uncertainty estimation functionality
#' (`predict.type = "se"`) for the randomForest, which is not provided by the
#' underlying package.
#'
#' Currently implemented methods are:
#'
#' \itemize{
#' \item If `se.method = "jackknife"` the standard error of a prediction is
#' estimated by computing the jackknife-after-bootstrap, the mean-squared
#' difference between the prediction made by only using trees which did not
#' contain said observation and the ensemble prediction.
#' \item If `se.method = "bootstrap"` the standard error of a prediction is
#' estimated by bootstrapping the random forest, where the number of bootstrap
#' replicates and the number of trees in the ensemble are controlled by
#' `se.boot` and `se.ntree` respectively, and then taking the standard deviation
#' of the bootstrap predictions. The "brute force" bootstrap is executed when
#' `ntree = se.ntree`, the latter of which controls the number of trees in the
#' individual random forests which are bootstrapped. The "noisy bootstrap" is
#' executed when `se.ntree < ntree` which is less computationally expensive. A
#' Monte-Carlo bias correction may make the latter option prefarable in many
#' cases. Defaults are `se.boot = 50` and `se.ntree = 100`.
#'
#' \item If `se.method = "sd"`, the default, the standard deviation of the
#' predictions across trees is returned as the variance estimate. This can be
#' computed quickly but is also a very naive estimator. }
#'
#' For both \dQuote{jackknife} and \dQuote{bootstrap}, a Monte-Carlo bias
#' correction is applied and, in the case that this results in a negative
#' variance estimate, the values are truncated at 0.
#'
#' Note that when using the \dQuote{jackknife} procedure for se estimation,
#' using a small number of trees can lead to training data observations that are
#' never out-of-bag. The current implementation ignores these observations, but
#' in the original definition, the resulting se estimation would be undefined.
#'
#' Please note that all of the mentioned `se.method` variants do not affect the
#' computation of the posterior mean \dQuote{response} value. This is always the
#' same as from the underlying randomForest.
#'
#' @section regr.featureless:
#'
#' A very basic baseline method which is useful for model comparisons (if you
#' don't beat this, you very likely have a problem).
#' Does not consider any features of the task and only uses the target feature
#' of the training data to make predictions.
#' Using observation weights is currently not supported.
#'
#' Methods \dQuote{mean} and \dQuote{median} always predict a constant value
#' for each new observation which corresponds to the observed mean or median of
#' the target feature in training data, respectively.
#'
#' The default method is \dQuote{mean} which corresponds to the ZeroR algorithm
#' from WEKA, see <https://weka.wikispaces.com/ZeroR>.
#'
#' @section classif.featureless:
#'
#' Method \dQuote{majority} predicts always the majority class for each new
#' observation. In the case of ties, one randomly sampled, constant class is predicted
#' for all observations in the test set.
#' This method is used as the default. It is very similar to the ZeroR classifier
#' from WEKA (see <https://weka.wikispaces.com/ZeroR>). The only difference is
#' that ZeroR always predicts the first class of the tied class values instead
#' of sampling them randomly.
#'
#' Method \dQuote{sample-prior} always samples a random class for each individual test
#' observation according to the prior probabilities observed in the training data.
#'
#' If you opt to predict probabilities, the class probabilities always
#' correspond to the prior probabilities observed in the training data.
#'
#' @return ([Learner]).
#' @family learner
#' @export
#' @aliases Learner
#' @examples
#' makeLearner("classif.rpart")
#' makeLearner("classif.lda", predict.type = "prob")
#' lrn = makeLearner("classif.lda", method = "t", nu = 10)
#' getHyperPars(lrn)
makeLearner = function(cl, id = cl, predict.type = "response", predict.threshold = NULL,
  fix.factors.prediction = FALSE, ..., par.vals = list(), config = list()) {

  assertString(cl)
  assertFlag(fix.factors.prediction)
  assertList(config, names = "named")
  if ("show.info" %in% names(config)) {
    stop("'show.info' cannot be set in 'makeLearner', please use 'configureMlr' instead.")
  }
  assertSubset(names(config), choices = names(getMlrOptions()))
  constructor = try(getS3method("makeRLearner", class = cl), silent = TRUE)
  if (inherits(constructor, "try-error")) {
    possibles = getNameProposals(cl, possible.inputs = suppressWarnings(listLearners()$class))
    stopf("Couldn't find learner '%s'\nDid you mean one of these learners instead: %s",
      cl, stri_flatten(possibles, collapse = " "))
  }
  wl = do.call(constructor, list())
  wl$config = config

  if (!missing(id)) {
    assertString(id)
    wl$id = id
  }

  # predict.threshold is checked in setter below
  assertList(par.vals, names = "unique")
  if (stri_isempty(cl)) {
    stop("Cannot create learner from empty string!")
  }
  if (!inherits(wl, "RLearner")) {
    stop("Learner must be a basic RLearner!")
  }
  wl = setHyperPars(learner = wl, ..., par.vals = par.vals)
  wl = setPredictType(learner = wl, predict.type = predict.type)
  if (!is.null(predict.threshold)) {
    wl = setPredictThreshold(wl, predict.threshold)
  }
  wl$fix.factors.prediction = fix.factors.prediction
  return(wl)
}
