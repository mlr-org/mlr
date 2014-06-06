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
#' @param properties [\code{character)}]\cr
#'   Set of required properties to filter for. Default is \code{character(0)}.
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
listLearners = function(type=NA, properties = character(0L), quiet=TRUE, warn.missing.packages=TRUE) {
  checkArg(type, choices=list("classif", "regr", "surv", NA), NA)
  checkArg(properties, "character", na.ok = FALSE)
  checkArg(warn.missing.packages, "logical", len=1L, na.ok=FALSE)

  meths = as.character(methods("makeRLearner"))
  # FIXME preallocate
  res = err = vector("list", length(meths))
  for (i in seq_along(meths)) {
    m = meths[[i]]
    if (quiet)
      suppressAll(lrn <- try(do.call(m, list()), silent=TRUE))
    else
      lrn = try(do.call(m, list()))

    if (is.error(lrn)) {
      err[[i]] = strsplit(as.character(m), "makeRLearner.")[[1L]][2L]
    } else if ((is.na(type) || type == lrn$type) && all(properties %in% lrn$properties)) {
        res[[i]] = lrn
    }
  }
  res = filterNull(res)
  err = filterNull(err)
  if (warn.missing.packages && length(err))
    warningf("The following learners could not be constructed, probably because their packages are not installed:\n%s\nCheck ?learners to see which packages you need or install mlr with all suggestions.", collapse(err))
  vcapply(res, function(lrn) class(lrn)[1L])
}

#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task. Learners are returned that are applicable.
#' @export
#' @rdname listLearners
listLearnersForTask = function(task, properties = character(0L), warn.missing.packages=TRUE) {
  checkArg(task, "SupervisedTask")
  checkArg(properties, "character", na.ok = FALSE)
  td = task$task.desc

  props = character(0L)
  if (td$n.feat["numerics"]) props = c(props, "numerics")
  if (td$n.feat["factors"]) props = c(props, "factors")
  if (td$n.feat["missings"]) props = c(props, "missings")
  if (td$type == "classif") {
    if (length(td$class.levels) == 1L) props = c(props, "oneclass")
    if (length(td$class.levels) == 2L) props = c(props, "twoclass")
    if (length(td$class.levels) >= 3L) props = c(props, "multiclass")
  }

  listLearners(type=td$type, properties = union(props, properties), warn.missing.packages=warn.missing.packages)
}
