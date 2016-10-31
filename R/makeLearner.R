#' @title Create learner object.
#'
#' @description
#' For a classification learner the \code{predict.type} can be set
#' to \dQuote{prob} to predict probabilities and the maximum
#' value selects the label. The threshold used to assign the label can later be changed using the
#' \code{\link{setThreshold}} function.
#'
#' To see all possible properties of a learner, go to: \code{\link{LearnerProperties}}.
#'
#' @template arg_lrncl
#' @param id [\code{character(1)}]\cr
#'   Id string for object. Used to display object.
#'   Default is \code{cl}.
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} (= labels) or \dQuote{prob} (= probabilities and labels by selecting the ones with maximal probability).
#'   Regression: \dQuote{response} (= mean response) or \dQuote{se} (= standard errors and mean response).
#'   Survival: \dQuote{response} (= some sort of orderable risk) or \dQuote{prob} (= time dependent probabilities).
#'   Clustering: \dQuote{response} (= cluster IDS) or \dQuote{prob} (= fuzzy cluster membership probabilities),
#'   Multilabel: \dQuote{response} (= logical matrix indicating the predicted class labels) or \dQuote{prob} (= probabilities and corresponding logical matrix indicating class labels).
#'   Default is \dQuote{response}.
#' @template arg_predictthreshold
#' @param fix.factors.prediction [\code{logical(1)}]\cr
#'   In some cases, problems occur in underlying learners for factor features during prediction.
#'   If the new features have LESS factor levels than during training (a strict subset),
#'   the learner might produce an  error like
#'   \dQuote{type of predictors in new data do not match that of the training data}.
#'   In this case one can repair this problem by setting this option to \code{TRUE}.
#'   We will simply add the missing factor levels missing from the test feature
#'   (but present in training) to that feature.
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Optional named (hyper)parameters.
#'   Alternatively these can be given using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameters. The arguments in
#'   \code{...} take precedence over values in this list. We strongly
#'   encourage you to use one or the other to pass (hyper)parameters
#'   to the learner but not both.
#' @param config [\code{named list}]\cr
#'   Named list of config option to overwrite global settings set via \code{\link{configureMlr}}
#'   for this specific learner.
#' @return [\code{\link{Learner}}].
#' @family learner
#' @export
#' @aliases Learner
#' @examples
#' makeLearner("classif.rpart")
#' makeLearner("classif.lda", predict.type = "prob")
#' lrn = makeLearner("classif.lda", method = "t", nu = 10)
#' print(lrn$par.vals)
makeLearner = function(cl, id = cl, ..., predict.type = "response", predict.threshold = NULL,
  fix.factors.prediction = FALSE, par.vals = list(), config = list()) {

  assertString(cl)
  assertFlag(fix.factors.prediction)
  assertList(config, names = "named")
  if ("show.info" %in% names(config))
    stop("'show.info' cannot be set in 'makeLearner', please use 'configureMlr' instead.")
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
  if (stri_isempty(cl))
    stop("Cannot create learner from empty string!")
  if (!inherits(wl, "RLearner"))
    stop("Learner must be a basic RLearner!")
  wl = setHyperPars(learner = wl, ..., par.vals = par.vals)
  wl = setPredictType(learner = wl, predict.type = predict.type)
  if (!is.null(predict.threshold))
    wl = setPredictThreshold(wl, predict.threshold)
  wl$fix.factors.prediction = fix.factors.prediction
  return(wl)
}

