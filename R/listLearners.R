#' Find matching learning algorithms.
#'
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc.
#'
#' The default for all search parameters is \code{NA}, meaning: property is not required, do not care.
#'
#' Note that the packages of all learners are loaded during the search.
#'
#' @param type [\code{character(1)}]\cr
#'   Type of the learning algorithm, one of \dQuote{classif}, \dQuote{regr}
#'   or \dQuote{surv}.
#' @param numerics [\code{logical(1)}]\cr
#'   Supports real-valued features?
#' @param factors [\code{logical(1)}]\cr
#'   Supports factor features?
#' @param missings [\code{logical(1)}]\cr
#'   Supports missing values in features?
#' @param weights [\code{logical(1)}]\cr
#'   Supports case weights?
#' @param oneclass [\code{logical(1)}]\cr
#'   Supports oneclass problems?
#' @param twoclass [\code{logical(1)}]\cr
#'   Supports twoclass problems?
#' @param multiclass [\code{logical(1)}]\cr
#'   Supports multiclass problems?
#' @param prob [\code{logical(1)}]\cr
#'   Can predict probabilities (classification)?
#' @param se [\code{logical(1)}]\cr
#'   Can predict standard errors (regression)?
#' @param quiet [\code{logical(1)}]\cr
#'   Construct learners quietly to check their properties, shows no package startup messages.
#'   Turn off if you suspect errors.
#'   Default is code{TRUE}.
#' @param warn.missing.packages [\code{logical(1)}]\cr
#'   If some learner cannot be constructed because its package is missing,
#'   should a warning be shown?
#'   Default is code{TRUE}.
#' @return [\code{character}]. Class names of matching learners.
#' @export
listLearners = function(type=NA, numerics=NA, factors=NA,
                        missings=NA, weights=NA,
                        oneclass=NA, twoclass=NA, multiclass=NA,
                        prob=NA, se=NA, quiet=TRUE,
                        warn.missing.packages=TRUE) {

  checkArg(type, choices=list("classif", "regr", "surv", NA), NA)
  checkArg(numerics, "logical", len=1L, na.ok=TRUE)
  checkArg(factors, "logical", len=1L, na.ok=TRUE)
  checkArg(missings, "logical", len=1L, na.ok=TRUE)
  checkArg(weights, "logical", len=1L, na.ok=TRUE)
  checkArg(oneclass, "logical", len=1L, na.ok=TRUE)
  checkArg(twoclass, "logical", len=1L, na.ok=TRUE)
  checkArg(multiclass, "logical", len=1L, na.ok=TRUE)
  checkArg(prob, "logical", len=1L, na.ok=TRUE)
  checkArg(se, "logical", len=1L, na.ok=TRUE)
  checkArg(warn.missing.packages, "logical", len=1L, na.ok=FALSE)

  meths = as.character(methods("makeRLearner"))
  # FIXME preallocate
  res = list()
  errs = list()
  for (m in meths) {
    if (quiet)
      suppressAll(lrn <- try(do.call(m, list()), silent=TRUE))
    else
      lrn = try(do.call(m, list()))
    if (is.error(lrn)) {
      errs = c(errs, strsplit(as.character(m), "makeRLearner.")[[1L]][2L])
    } else {
      if (
        ( is.na(type) || type == lrn$type ) &&
          ( is.na(numerics) || numerics == lrn$numerics ) &&
          ( is.na(factors) || factors == lrn$factors ) &&
          ( is.na(missings) || missings == lrn$missings ) &&
          ( is.na(weights) || weights == lrn$weights  ) &&
          ( is.na(oneclass) || oneclass == lrn$oneclass ) &&
          ( is.na(twoclass) || twoclass == lrn$twoclass ) &&
          ( is.na(multiclass) || multiclass == lrn$multiclass ) &&
          ( is.na(prob) || prob == lrn$prob ) &&
          ( is.na(se) || se == lrn$se )
      ) {
        res[[length(res)+1L]] = lrn
      }
    }
  }
  if (warn.missing.packages && length(errs))
    warningf("The following learners could not be constructed, probably because their packages are not installed:\n%s\nCheck ?learners to see which packages you need or install mlr with all suggestions.", collapse(errs))
  sapply(res, function(lrn) class(lrn)[1L])
}

#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task. Learners are returned that are applicable.
#' @export
#' @rdname listLearners
listLearnersForTask = function(task, weights=NA, prob=NA, se=NA, warn.missing.packages=TRUE) {
  checkArg(task, "SupervisedTask")
  td = task$task.desc

  numerics = ifelse(td$n.feat["numerics"] > 0L, TRUE, NA)
  factors = ifelse(td$n.feat["factors"] > 0L, TRUE, NA)
  missings = ifelse(td$has.missings, TRUE, NA)
  oneclass = ifelse(td$type=="classif" && length(td$class.levels) == 1L, TRUE, NA)
  twoclass = ifelse(td$type=="classif" && length(td$class.levels) == 2L, TRUE, NA)
  multiclass = ifelse(td$type=="classif" && length(td$class.levels) > 2L, TRUE, NA)

  listLearners(type=td$type, numerics=numerics, factors=factors,
               missings=missings, weights=weights,
               oneclass=oneclass, twoclass=twoclass, multiclass=multiclass,
               prob=prob, se=se, warn.missing.packages=warn.missing.packages)
}
