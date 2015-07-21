#' @title Create control structures for feature selection.
#'
#' @description
#' Feature selection method used by \code{\link{selectFeatures}}.
#' The following methods are available:
#'
#'  \describe{
#'    \item{FeatSelControlExhaustive}{Exhaustive search. All feature sets (up to a certain number
#'      of features \code{max.features}) are searched.}
#'    \item{FeatSelControlRandom}{Random search. Features vectors are randomly drawn,
#'      up to a certain number of features \code{max.features}.
#'      A feature is included in the current set with probability \code{prob}.
#'      So we are basically drawing (0,1)-membership-vectors, where each element
#'      is Bernoulli(\code{prob}) distributed.}
#'    \item{FeatSelControlSequential}{Deterministic forward or backward search. That means extending
#'      (forward) or shrinking (backward) a feature set.
#'      Depending on the given \code{method} different approaches are taken.\cr
#'      \code{sfs} Sequential Forward Search: Starting from an empty model, in each step the feature increasing
#'      the performance measure the most is added to the model.\cr
#'      \code{sbs} Sequential Backward Search: Starting from a model with all features, in each step the feature
#'      decreasing the performance measure the least is removed from the model.\cr
#'      \code{sffs} Sequential Floating Forward Search: Starting from an empty model, in each step the algorithm
#'      chooses the best model from all models with one additional feature and from all models with one
#'      feature less.\cr
#'      \code{sfbs} Sequential Floating Backward Search: Similar to \code{sffs} but starting with a full model.}
#'    \item{FeatSelControlGA}{Search via genetic algorithm.
#'      The GA is a simple (\code{mu}, \code{lambda}) or (\code{mu} + \code{lambda}) algorithm,
#'      depending on the \code{comma} setting.
#'      A comma strategy selects a new population of size \code{mu} out of the
#'      \code{lambda} > \code{mu} offspring.
#'      A plus strategy uses the joint pool of \code{mu} parents and \code{lambda} offspring
#'      for selecting \code{mu} new candidates.
#'      Out of those \code{mu} features, the new \code{lambda} features are generated
#'      by randomly choosing pairs of parents. These are crossed over and \code{crossover.rate}
#'      represents the probability of choosing a feature from the first parent instead of
#'      the second parent.
#'      The resulting offspring is mutated, i.e., its bits are flipped with
#'      probability \code{mutation.rate}. If \code{max.features} is set, offspring are
#'      repeatedly generated until the setting is satisfied.}
#'  }
#'
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @template arg_imputey
#' @param maxit [\code{integer(1)}]\cr
#'   Maximal number of iterations. Note, that this is usually not equal to the number
#'   of function evaluations.
#' @param max.features [\code{integer(1)}]\cr
#'   Maximal number of features.
#' @param tune.threshold [\code{logical(1)}]\cr
#'   Should the threshold be tuned for the measure at hand, after each feature set evaluation,
#'   via \code{\link{tuneThreshold}}?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is \code{FALSE}.
#' @param tune.threshold.args [\code{list}]\cr
#'   Further arguments for threshold tuning that are passed down to \code{\link{tuneThreshold}}.
#'   Default is none.
#' @param log.fun [\code{function} | \code{NULL}]\cr
#'   Function used for logging. If set to \code{NULL}, the internal default will be used.
#'   Otherwise a function with arguments \code{learner}, \code{resampling}, \code{measures},
#'   \code{par.set}, \code{control}, \code{opt.path}, \code{dob}, \code{x}, \code{y}, \code{remove.nas},
#'   and \code{stage} is expected.
#'   The default displays the performance measures, the time needed for evaluating,
#'   the currently used memory and the max memory ever used before
#'   (the latter two both taken from \code{\link{gc}}).
#'   See the implementation for details.
#' @param prob [\code{numeric(1)}]\cr
#'   Parameter of the random feature selection. Probability of choosing a feature.
#' @param method [\code{character(1)}]\cr
#'   Parameter of the sequential feature selection. A character representing the method. Possible
#'   values are \code{sfs} (forward search), \code{sbs} (backward search), \code{sffs}
#'   (floating forward search) and \code{sfbs} (floating backward search).
#' @param alpha [\code{numeric(1)}]\cr
#'   Parameter of the sequential feature selection.
#'   Minimal required value of improvement difference for a forward / adding step.
#'   Default is 0.01.
#' @param beta [\code{numeric(1)}]\cr
#'   Parameter of the sequential feature selection.
#'   Minimal required value of improvement difference for a backward / removing step.
#'   Negative values imply that you allow a slight decrease for the removal of a feature.
#'   Default is -0.001.
#' @param mu [\code{integer(1)}]\cr
#'   Parameter of the GA feature selection. Size of the parent population.
#' @param lambda [\code{integer(1)}]\cr
#'   Parameter of the GA feature selection. Size of the children population (should be smaller
#'   or equal to \code{mu}).
#' @param crossover.rate [\code{numeric(1)}]\cr
#'   Parameter of the GA feature selection. Probability of choosing a bit from the first parent
#'   within the crossover mutation.
#' @param mutation.rate [\code{numeric(1)}]\cr
#'   Parameter of the GA feature selection. Probability of flipping a feature bit, i.e. switch
#'   between selecting / deselecting a feature.
#' @param comma [\code{logical(1)}]\cr
#'   Parameter of the GA feature selection, indicating whether to use a (\code{mu}, \code{lambda})
#'   or (\code{mu} + \code{lambda}) GA. The default is \code{FALSE}.
#' @return [\code{\link{FeatSelControl}}]. The specific subclass is one of
#'   \code{\link{FeatSelControlExhaustive}}, \code{\link{FeatSelControlRandom}},
#'   \code{\link{FeatSelControlSequential}}, \code{\link{FeatSelControlGA}}.
#' @family featsel
#' @name FeatSelControl
#' @rdname FeatSelControl
#' @aliases FeatSelControlExhaustive FeatSelControlRandom FeatSelControlSequential FeatSelControlGA
NULL

makeFeatSelControl = function(same.resampling.instance, impute.val = NULL, maxit, max.features,
  tune.threshold = FALSE, tune.threshold.args = list(), log.fun = NULL, ..., cl) {

  maxit = asCount(maxit, na.ok = TRUE, positive = TRUE)
  max.features = asCount(max.features, na.ok = TRUE, positive = TRUE)
  if (is.null(log.fun))
    log.fun = logFunFeatSel
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
  if (is.na(x$max.features))
    catf("Max. features: <not used>")
  else
    catf("Max. features: %i", x$max.features)
  catf("Max. iterations: %i", x$maxit)
  catf("Tune threshold: %s", x$tune.threshold)
  if (length(x$extra.args))
    catf("Further arguments: %s", convertToShortString(x$extra.args))
  else
    catf("Further arguments: <not used>")
}

