#' @title Create control structures for feature selection.
#'
#' @description
#' Feature selection method used by [selectFeatures].\cr
#' The methods used here follow a wrapper approach, described in
#' Kohavi and John (1997) (see references).
#'
#' The following optimization algorithms are available:
#'  \describe{
#'    \item{FeatSelControlExhaustive}{Exhaustive search. All feature sets (up to a certain number
#'      of features `max.features`) are searched.}
#'    \item{FeatSelControlRandom}{Random search. Features vectors are randomly drawn,
#'      up to a certain number of features `max.features`.
#'      A feature is included in the current set with probability `prob`.
#'      So we are basically drawing (0,1)-membership-vectors, where each element
#'      is Bernoulli(`prob`) distributed.}
#'    \item{FeatSelControlSequential}{Deterministic forward or backward search. That means extending
#'      (forward) or shrinking (backward) a feature set.
#'      Depending on the given `method` different approaches are taken.\cr
#'      `sfs` Sequential Forward Search: Starting from an empty model, in each step the feature increasing
#'      the performance measure the most is added to the model.\cr
#'      `sbs` Sequential Backward Search: Starting from a model with all features, in each step the feature
#'      decreasing the performance measure the least is removed from the model.\cr
#'      `sffs` Sequential Floating Forward Search: Starting from an empty model, in each step the algorithm
#'      chooses the best model from all models with one additional feature and from all models with one
#'      feature less.\cr
#'      `sfbs` Sequential Floating Backward Search: Similar to `sffs` but starting with a full model.}
#'    \item{FeatSelControlGA}{Search via genetic algorithm.
#'      The GA is a simple (`mu`, `lambda`) or (`mu` + `lambda`) algorithm,
#'      depending on the `comma` setting.
#'      A comma strategy selects a new population of size `mu` out of the
#'      `lambda` > `mu` offspring.
#'      A plus strategy uses the joint pool of `mu` parents and `lambda` offspring
#'      for selecting `mu` new candidates.
#'      Out of those `mu` features, the new `lambda` features are generated
#'      by randomly choosing pairs of parents. These are crossed over and `crossover.rate`
#'      represents the probability of choosing a feature from the first parent instead of
#'      the second parent.
#'      The resulting offspring is mutated, i.e., its bits are flipped with
#'      probability `mutation.rate`. If `max.features` is set, offspring are
#'      repeatedly generated until the setting is satisfied.}
#'  }
#'
#' @param same.resampling.instance (`logical(1)`)\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is `TRUE`.
#' @template arg_imputey
#' @param maxit (`integer(1)`)\cr
#'   Maximal number of iterations. Note, that this is usually not equal to the number
#'   of function evaluations.
#' @param max.features (`integer(1)`)\cr
#'   Maximal number of features.
#' @param tune.threshold (`logical(1)`)\cr
#'   Should the threshold be tuned for the measure at hand, after each feature set evaluation,
#'   via [tuneThreshold]?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is `FALSE`.
#' @param tune.threshold.args ([list])\cr
#'   Further arguments for threshold tuning that are passed down to [tuneThreshold].
#'   Default is none.
#' @template arg_log_fun
#' @param prob (`numeric(1)`)\cr
#'   Parameter of the random feature selection. Probability of choosing a feature.
#' @param method (`character(1)`)\cr
#'   Parameter of the sequential feature selection. A character representing the method. Possible
#'   values are `sfs` (forward search), `sbs` (backward search), `sffs`
#'   (floating forward search) and `sfbs` (floating backward search).
#' @param alpha (`numeric(1)`)\cr
#'   Parameter of the sequential feature selection.
#'   Minimal required value of improvement difference for a forward / adding step.
#'   Default is 0.01.
#' @param beta (`numeric(1)`)\cr
#'   Parameter of the sequential feature selection.
#'   Minimal required value of improvement difference for a backward / removing step.
#'   Negative values imply that you allow a slight decrease for the removal of a feature.
#'   Default is -0.001.
#' @param mu (`integer(1)`)\cr
#'   Parameter of the GA feature selection. Size of the parent population.
#' @param lambda (`integer(1)`)\cr
#'   Parameter of the GA feature selection. Size of the children population (should be smaller
#'   or equal to `mu`).
#' @param crossover.rate (`numeric(1)`)\cr
#'   Parameter of the GA feature selection. Probability of choosing a bit from the first parent
#'   within the crossover mutation.
#' @param mutation.rate (`numeric(1)`)\cr
#'   Parameter of the GA feature selection. Probability of flipping a feature bit, i.e. switch
#'   between selecting / deselecting a feature.
#' @param comma (`logical(1)`)\cr
#'   Parameter of the GA feature selection, indicating whether to use a (`mu`, `lambda`)
#'   or (`mu` + `lambda`) GA. The default is `FALSE`.
#' @return ([FeatSelControl]). The specific subclass is one of
#'   [FeatSelControlExhaustive], [FeatSelControlRandom],
#'   [FeatSelControlSequential], [FeatSelControlGA].
#' @references Ron Kohavi and George H. John,
#' Wrappers for feature subset selection, Artificial Intelligence Volume 97, 1997, 273-324.
#' <http://ai.stanford.edu/~ronnyk/wrappersPrint.pdf>.\cr
#' @family featsel
#' @name FeatSelControl
#' @rdname FeatSelControl
#' @aliases FeatSelControlExhaustive FeatSelControlRandom FeatSelControlSequential FeatSelControlGA
NULL

makeFeatSelControl = function(same.resampling.instance, impute.val = NULL, maxit, max.features,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = "default", ..., cl) {
  maxit = asCount(maxit, na.ok = TRUE, positive = TRUE)
  max.features = asCount(max.features, na.ok = TRUE, positive = TRUE)
  if (identical(log.fun, "default")) {
    log.fun = logFunFeatSel
  } else if (identical(log.fun, "memory")) {
    log.fun = logFunTuneMemory
  }
  x = makeOptControl(same.resampling.instance, impute.val, tune.threshold, tune.threshold.args, log.fun, ...)
  x$maxit = maxit
  x$max.features = max.features
  addClasses(x, c(cl, "FeatSelControl"))
}

#' @export
print.FeatSelControl = function(x, ...) {
  catf("FeatSel control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %s", ifelse(is.null(x$impute.val), "<worst>", sprintf("%g", x$impute.val)))
  if (is.na(x$max.features)) {
    catf("Max. features: <not used>")
  } else {
    catf("Max. features: %i", x$max.features)
  }
  catf("Max. iterations: %i", x$maxit)
  catf("Tune threshold: %s", x$tune.threshold)
  if (length(x$extra.args)) {
    catf("Further arguments: %s", convertToShortString(x$extra.args))
  } else {
    catf("Further arguments: <not used>")
  }
}
