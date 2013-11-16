#' Create learner object.
#'
#' For a classification learner the \code{predict.type} can be set
#' to \dQuote{prob} to predict probabilities and the maximum
#' value selects the label. The threshold used to assign the label can later be changed using the
#' \code{\link{setThreshold}} function.
#'
#' @param cl [\code{character(1)}]\cr
#'   Class of learner to create. By convention, all classification learners
#'   start with \dQuote{classif.} and all regression learners with
#'   \dQuote{regr.}. A list of all learners is available on the
#'   \code{\link{learners}} help page.
#' @param id [\code{character(1)}]\cr
#'   Id string for object. Used to display object.
#'   Default is \code{cl}.
#' @param predict.type [\code{character(1)}]\cr
#'   Classification: \dQuote{response} (= labels) or \dQuote{prob} (= probabilities and labels by selecting the ones with maximal probability).
#'   Regression: \dQuote{response} (= mean response) or \dQuote{se} (= standard errors and mean response).
#'   Default is \dQuote{response}.
#' @param ... [any]\cr
#'   Optional named (hyper)parameters.
#'   Alternatively these can be given using the \code{par.vals} argument.
#' @param par.vals [\code{list}]\cr
#'   Optional list of named (hyper)parameters. The arguments in
#'   \code{...} take precedence over values in this list. We strongly
#'   encourage you to use one or the other to pass (hyper)parameters
#'   to the learner but not both.
#' @return [\code{\link{Learner}}].
#' @export
#' @aliases Learner
#' @seealso [\code{\link{resample}}], [\code{\link{predict.WrappedModel}}]
#' @examples
#' makeLearner("classif.rpart")
#' makeLearner("classif.lda", predict.type = "prob")
#'
#' lrn <- makeLearner("classif.rpart", minsplit = 5)
#' print(lrn$par.vals)
#' lrn <- makeLearner("classif.lda", method = "mle")
#' print(lrn$par.vals)
#' lrn <- makeLearner("classif.lda", method = "t", nu = 10)
#' print(lrn$par.vals)
makeLearner = function(cl, id=cl, predict.type="response", ..., par.vals=list()) {
  checkArg(cl, "character", len=1L, na.ok=FALSE)
  constructor = getS3method("makeRLearner", class=cl)
  # FIXME wl = constructor()
  wl = do.call(constructor, list())

  if (!missing(id)) {
    checkArg(id, "character", len=1L, na.ok=FALSE)
    wl$id = id
  }
  checkArg(par.vals, "list")
  if (cl == "")
    stop("Cannot create learner from empty string!")
  if (!inherits(wl, "RLearner"))
    stop("Learner must be a basic RLearner!")
  wl = setHyperPars(wl, ..., par.vals=par.vals)
  wl = setPredictType(wl, predict.type)
  return(wl)
}

#' @S3method print Learner
print.Learner = function(x, ...) {
  cat(
    "Learner ", x$id, " from package ", collapse(x$package), "\n",
    "Type: ", x$type, "\n",
    "Class: ", class(x)[1L], "\n",
    "Predict-Type: ", x$predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(x), "\n\n",
    "Supported features Numerics:", x$numerics, " Factors:", x$factors, "\n",
    "Supports missings: ", x$missings, "\n",
    "Supports weights: ", x$weights, "\n",
    sep =""
  )
  if (x$type == "classif") {
    catf("Supports classes: %s",
         collapse(c("one", "two", "multi")[c(x$oneclass, x$twoclass, x$multiclass)]))
    catf("Supports probabilities: %s", x$prob)
  } else if (x$type == "regr") {
    catf("Supports standard errs: %s", x$se)
  }
}
