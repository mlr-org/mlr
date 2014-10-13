#' @title Find matching learning algorithms.
#'
#' @description
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc.
#'
#' Note that the packages of all learners are loaded during the search.
#'
#' Note that for general cost-sensitive learning, mlr currently supports mainly
#' \dQuote{wrapper} approaches like \code{\link{CostSensWeightedPairsWrapper}},
#' which are not listed, as they are not basic R learning algorithms.
#'
#' @template arg_task_or_type
#' @param properties [\code{character}]\cr
#'   Set of required properties to filter for. Default is \code{character(0)}.
#' @param quiet [\code{logical(1)}]\cr
#'   Construct learners quietly to check their properties, shows no package startup messages.
#'   Turn off if you suspect errors.
#'   Default is \code{TRUE}.
#' @param warn.missing.packages [\code{logical(1)}]\cr
#'   If some learner cannot be constructed because its package is missing,
#'   should a warning be shown?
#'   Default is \code{TRUE}.
#' @param create [\code{logical(1)}]\cr
#'   Instantiate objects (or return strings)?
#'   Default is \code{FALSE}.
#' @return [\code{character} | \code{list} of \code{\link{Learner}}]. Class names of matching
#'   learners or instantiated objects.
#' @examples
#' \dontrun{
#' listLearners("classif", properties = c("multiclass", "prob"))
#' data = iris
#' task = makeClassifTask(data = data, target = "Species")
#' listLearners(task)
#' }
#' @export
listLearners  = function(obj = NA_character_, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  if (!missing(obj))
    assert(checkCharacter(obj), checkClass(obj, "Task"))
  assertCharacter(properties, any.missing = FALSE)
  assertFlag(warn.missing.packages)
  assertFlag(create)
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

  assertChoice(obj, choices = c("classif", "regr", "surv", "costsens", "cluster", NA_character_))
  type = obj
  meths = as.character(methods("makeRLearner"))
  res = err = vector("list", length(meths))
  learner.classes = vcapply(strsplit(meths, "makeRLearner\\."), function(x) x[2L])
  for (i in seq_along(meths)) {
    cl = learner.classes[[i]]
    if (quiet)
      suppressAll(lrn <- try(makeLearner(cl), silent = TRUE))
    else
      lrn = try(makeLearner(cl))

    if (is.error(lrn)) {
      err[[i]] = cl
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
listLearners.Task = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, create = FALSE) {

  task = obj
  assertCharacter(properties, any.missing = FALSE)
  td = task$task.desc

  props = character(0L)
  if (td$n.feat["numerics"] > 0L) props = c(props, "numerics")
  if (td$n.feat["factors"] > 0L) props = c(props, "factors")
  if (td$n.feat["ordered"] > 0L) props = c(props, "ordered")
  if (td$has.missings) props = c(props, "missings")
  if (td$type == "classif") {
    if (length(td$class.levels) == 1L) props = c(props, "oneclass")
    if (length(td$class.levels) == 2L) props = c(props, "twoclass")
    if (length(td$class.levels) >= 3L) props = c(props, "multiclass")
  }

  listLearners.character(td$type, union(props, properties), quiet, warn.missing.packages, create)
}
