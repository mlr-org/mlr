#' @title Find matching learning algorithms.
#'
#' @description
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc.
#'
#' Note that the packages of all learners are loaded during the search.
#'
#' @template arg_task_or_type
#' @param properties [\code{character)}]\cr
#'   Set of required properties to filter for. Default is \code{character(0)}.
#' @param quiet [\code{logical(1)}]\cr
#'   Construct learners quietly to check their properties, shows no package startup messages.
#'   Turn off if you suspect errors.
#'   Default is \code{TRUE}.
#' @param warn.missing.packages [\code{logical(1)}]\cr
#'   If some learner cannot be constructed because its package is missing,
#'   should a warning be shown?
#'   Default is \code{TRUE}.
#' @param warn.missing.packages [\code{logical(1)}]\cr
#'   If some learner cannot be constructed because its package is missing,
#'   should a warning be shown?
#'   Default is \code{TRUE}.
#' @param create [\code{logical(1)}]\cr
#'   Instantiate objects (or return strings)?
#'   Default is \code{FALSE}.
#' @return [\code{character} | \code of \code{\link{Learner}}]. Class names of matching
#'   learners or instantiated objects.
#' @export
listLearners  = function(obj = NA_character_, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  if (!missing(obj))
    checkArg(obj, c("character", "SupervisedTask"))
  checkArg(properties, "character", na.ok = FALSE)
  checkArg(warn.missing.packages, "logical", len = 1L, na.ok = FALSE)
  checkArg(create, "logical", len = 1L, na.ok = FALSE)
  UseMethod("listLearners")
}


#' @export
#' @rdname listLearners
listLearners.default  = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  listLearners.character(obj = NA_character_, properties, quiet, warn.missing.packages, create)
}

#' @export
#' @rdname listLearners
listLearners.character  = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  checkArg(obj, choices = c("classif", "regr", "surv", "costsens", NA_character_))
  type = obj
  meths = as.character(methods("makeRLearner"))
  res = err = vector("list", length(meths))
  for (i in seq_along(meths)) {
    m = meths[[i]]
    if (quiet)
      suppressAll(lrn <- try(do.call(m, list()), silent = TRUE))
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
  if (create)
    return(res)
  else
    return(vcapply(res, getClass1))
}

#' @export
#' @rdname listLearners
listLearners.SupervisedTask = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  task = obj
  checkArg(properties, "character", na.ok = FALSE)
  td = task$task.desc

  props = character(0L)
  if (td$n.feat["numerics"] > 0L) props = c(props, "numerics")
  if (td$n.feat["factors"] > 0L) props = c(props, "factors")
  if (td$has.missings) props = c(props, "missings")
  if (td$type == "classif") {
    if (length(td$class.levels) == 1L) props = c(props, "oneclass")
    if (length(td$class.levels) == 2L) props = c(props, "twoclass")
    if (length(td$class.levels) >= 3L) props = c(props, "multiclass")
  }

  listLearners.character(td$type, union(props, properties), quiet, warn.missing.packages, create)
}
