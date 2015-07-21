#' @title Find matching learning algorithms.
#'
#' @description
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc.
#'
#' Note that the packages of all learners are loaded during the search if you create them.
#' This can be a lot. If you do not create them we only inspect properties of the S3 classes.
#' This will be a lot faster.
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
#' @param check.packages [\code{logical(1)}]\cr
#'   Check if required packages are installed. Calls
#'   \code{installed.packages()}. If \code{create} is \code{TRUE},
#'   this is done implicitly and the value of this parameter is ignored.
#'   Default is \code{TRUE}. If set to \code{FALSE}, learners that cannot
#'   actually be constructed because of missing packages may be returned.
#' @param create [\code{logical(1)}]\cr
#'   Instantiate objects (or return strings)?
#'   Default is \code{FALSE}.
#' @return [\code{character} | \code{list} of \code{\link{Learner}}].
#'   The latter is named by ids of listed learners.
#' @examples
#' \dontrun{
#' listLearners("classif", properties = c("multiclass", "prob"))
#' data = iris
#' task = makeClassifTask(data = data, target = "Species")
#' listLearners(task)
#' }
#' @export
listLearners  = function(obj = NA_character_, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, check.packages = TRUE, create = FALSE) {

  if (!missing(obj))
    assert(checkCharacter(obj), checkClass(obj, "Task"))
  assertSubset(properties, getSupportedLearnerProperties())
  assertFlag(warn.missing.packages)
  assertFlag(create)
  UseMethod("listLearners")
}


#' @export
#' @rdname listLearners
listLearners.default  = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, check.packages = TRUE, create = FALSE) {

  listLearners.character(obj = NA_character_, properties, quiet, warn.missing.packages, check.packages, create)
}

#' @export
#' @rdname listLearners
listLearners.character  = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, check.packages = TRUE, create = FALSE) {

  assertChoice(obj, choices = c("classif", "regr", "surv", "costsens", "cluster", "multilabel", NA_character_))
  assertSubset(properties, getSupportedLearnerProperties(obj))

  type = obj
  meths = as.character(methods("makeRLearner"))
  # make really sure we filter out our own mock learners here, they have a very unique name
  meths = meths[!grepl("__mlrmocklearners__", meths)]
  res = err = vector("list", length(meths))
  learner.classes = vcapply(strsplit(meths, "makeRLearner\\."), function(x) x[2L])
  if (!create && check.packages) {
    installed.packs = rownames(installed.packages())
  }
  for (i in seq_along(meths)) {
    cl = learner.classes[[i]]
    lrn.type = strsplit(cl, "\\.")[[1L]][1L]
    m = meths[[i]]
    mb = functionBody(m)
    if (!create && check.packages) {
      depsInstalled = all(sapply(eval(mb[[2L]]$package), function(x) {
        char = substr(x, 1L, 1L)
        pack = substr(x, 1L + (char == "!" | char == "_"), nchar(x))
        pack %in% installed.packs
      }))
    }
    if (create || !check.packages || depsInstalled) {
      lrn.properties = getLearnerProperties(mb[[2L]])
      lrn.properties = eval(lrn.properties)
      lrn = cl
    } else {
      err[[i]] = learner.classes[[i]]
    }

    # check if we have correct type and props
    if (is.null(err[[i]]) && (is.na(type) || type == lrn.type) && all(properties %in% lrn.properties)) {
      if (create) {
        if (quiet) {
          suppressAll(lrn <- try(makeLearner(lrn), silent = TRUE))
        } else {
          lrn = try(makeLearner(lrn))
        }
        if (is.error(lrn)) {
          err[[i]] = cl
        }
      }
      if (is.null(err[[i]])) {
        res[[i]] = lrn
      }
    }
  }

  res = filterNull(res)
  if (create)
    names(res) = extractSubList(res, "id")
  else
    res = unlist(res)
  err = filterNull(err)
  if (warn.missing.packages && length(err))
    warningf("The following learners could not be constructed, probably because their packages are not installed:\n%s\nCheck ?learners to see which packages you need or install mlr with all suggestions.", collapse(err))
  return(res)
}

#' @export
#' @rdname listLearners
listLearners.Task = function(obj, properties = character(0L),
  quiet = TRUE, warn.missing.packages = TRUE, check.packages = TRUE, create = FALSE) {

  task = obj
  td = getTaskDescription(task)

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

  listLearners.character(td$type, union(props, properties), quiet, warn.missing.packages, check.packages, create)
}
