#' @title Impute and re-impute data
#'
#' @description
#' Allows imputation of missing feature values through various techniques.
#' Note that you have the possibility to re-impute a data set
#' in the same way as the imputation was performed during training.
#' This especially comes in handy during resampling when one wants to perform the
#' same imputation on the test set as on the training set.
#'
#' The function \code{impute} performs the imputation on a data set and returns,
#' alongside with the imputed data set, an \dQuote{ImputationDesc} object
#' which can contain \dQuote{learned} coefficients and helpful data.
#' It can then be passed together with a new data set to \code{\link{reimpute}}.
#'
#' The imputation techniques can be specified for certain features or for feature classes,
#' see function arguments.
#'
#' You can either provide an arbitrary object, use a built-in imputation method listed
#' under \code{\link{imputations}} or create one yourself using \code{\link{makeImputeMethod}}.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{features [\code{character}]}{Feature names, these are the column names of \code{data},
#'     excluding \code{target}.}
#'   \item{lvls [\code{named list}]}{Mapping of column names of factor features to their levels,
#'     including newly created ones during imputation.}
#'   \item{impute [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{dummies [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{impute.new.levels [\code{logical(1)}]}{See argument.}
#'   \item{recode.factor.levels [\code{logical(1)}]}{See argument.}
#' }
#'
#' @param data [\code{data.frame}]\cr
#'   Input data.
#' @param target [\code{character}]\cr
#'   Name of the column(s) specifying the response.
#'   Default is \code{character(0)}.
#' @param classes [\code{named list}]\cr
#'   Named list containing imputation techniques for classes of columns.
#'   E.g. \code{list(numeric = imputeMedian())}.
#' @param cols [\code{named list}]\cr
#'   Named list containing names of imputation methods to impute missing values
#'   in the data column referenced by the list element's name. Overrules imputation set via
#'   \code{classes}.
#' @param dummy.classes [\code{character}]\cr
#'   Classes of columns to create dummy columns for.
#'   Default is \code{character(0)}.
#' @param dummy.cols [\code{character}]\cr
#'   Column names to create dummy columns (containing binary missing indicator) for.
#'   Default is \code{character(0)}.
#' @param dummy.type [\code{character(1)}]\cr
#'   How dummy columns are encoded. Either as 0/1 with type \dQuote{numeric}
#'   or as \dQuote{factor}.
#'   Default is \dQuote{factor}.
#' @param force.dummies [\code{logical(1)}]\cr
#'   Force dummy creation even if the respective data column does not
#'   contain any NAs. Note that (a) most learners will complain about
#'   constant columns created this way but (b) your feature set might
#'   be stochastic if you turn this off.
#'   Default is \code{FALSE}.
#' @param impute.new.levels [\code{logical(1)}]\cr
#'   If new, unencountered factor level occur during reimputation,
#'   should these be handled as NAs and then be imputed the same way?
#'   Default is \code{TRUE}.
#' @param recode.factor.levels [\code{logical(1)}]\cr
#'   Recode factor levels after reimputation, so they match the respective element of
#'   \code{lvls} (in the description object) and therefore match the levels of the
#'   feature factor in the training data after imputation?.
#'   Default is \code{TRUE}.
#' @return [\code{list}]
#'   \item{data [\code{data.frame}]}{Imputed data.}
#'   \item{desc [\code{ImputationDesc}]}{Description object.}
#' @export
#' @family impute
#' @examples
#' df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z = 1:3)
#' imputed = impute(df, target = character(0), cols = list(x = 99, y = imputeMode()))
#' print(imputed$data)
#' reimpute(data.frame(x = NA), imputed$desc)
impute = function(data, target = character(0L), classes = list(), cols = list(),
  dummy.classes = character(0L), dummy.cols = character(0L), dummy.type = "factor",
  force.dummies = FALSE, impute.new.levels = TRUE, recode.factor.levels = TRUE) {

  allowed.classes = c("logical", "integer", "numeric", "complex", "character", "factor")
  assertDataFrame(data)
  assertCharacter(target, any.missing = FALSE)
  if (length(target)) {
    not.ok = which.first(target %nin% names(data))
    if (length(not.ok))
      stopf("Target column '%s' must be present in data", target[not.ok])
    not.ok = which.first(target %in% names(cols))
    if (length(not.ok))
      stopf("Imputation of target column '%s' not possible", target[not.ok])
    not.ok = which.first(target %in% names(dummy.cols))
    if (length(not.ok))
      stopf("Dummy creation of target column '%s' not possible", target[not.ok])
  }
  assertList(classes)
  not.ok = which.first(names(classes) %nin% allowed.classes)
  if (length(not.ok))
    stopf("Column class '%s' for imputation not recognized", names(classes)[not.ok])
  assertList(cols)
  not.ok = which.first(names(cols) %nin% names(data))
  if (length(not.ok))
    stopf("Column for imputation not present in data: %s", names(cols)[not.ok])
  assertSubset(dummy.classes, choices = allowed.classes)
  assertCharacter(dummy.cols, any.missing = FALSE)
  not.ok = which.first(dummy.cols %nin% names(data))
  if (length(not.ok))
    stopf("Column for dummy creation not present in data: %s", dummy.cols[not.ok])
  assertCharacter(dummy.classes, any.missing = FALSE)
  assertFlag(force.dummies)
  assertChoice(dummy.type, c("factor", "numeric"))
  assertFlag(impute.new.levels)
  assertFlag(recode.factor.levels)

  features = setdiff(names(data), target)
  feature.classes = vcapply(data[features], function(x) class(x)[1L])

  # some elements need to be set to a non-harmful
  # setting for the first iteration of impute
  # those will be overwritten later
  desc = makeS3Obj("ImputationDesc",
    target = target,
    features = features,
    lvls = NULL,
    impute = namedList(features),
    dummies = character(0L),
    dummy.type = dummy.type,
    force.dummies = force.dummies,
    impute.new.levels = FALSE,
    recode.factor.levels = FALSE
  )

  # handle classes -> insert into desc
  cl2 = feature.classes[feature.classes %in% names(classes)]
  desc$impute[names(cl2)] = classes[cl2]

  # handle cols -> insert into desc
  desc$impute[names(cols)] = cols

  # handle dummies
  desc$dummies = union(names(feature.classes[feature.classes %in% dummy.classes]), dummy.cols)
  if (!desc$force.dummies)
    desc$dummies = desc$dummies[vlapply(data[desc$dummies], anyMissing)]

  # cleanup
  desc$impute = Filter(Negate(is.null), desc$impute)

  # learn and thereby transform to list(impute(...), args(...))
  desc$impute = Map(function(xn, x) {
    if (class(x)[1L] != "ImputeMethod")
      x = imputeConstant(x)
    list(
      impute = x$impute,
      args = do.call(x$learn, c(x$args, list(data = data, target = target, col = xn)))
    )
  }, xn = names(desc$impute), x = desc$impute)

  data = reimpute(data, desc)

  # store factor levels (this might include new levels created during imputation)
  ind = names(which(feature.classes == "factor"))
  desc$lvls = lapply(data[ind], levels)

  # set variables for consecutive imputes
  desc$recode.factor.levels = recode.factor.levels
  desc$impute.new.levels = impute.new.levels

  list(data = data, desc = desc)
}

#' @export
print.ImputationDesc = function(x, ...) {
  catf("Imputation description")
  catf("Target: %s", collapse(x$target))
  catf("Features: %i; Imputed: %i", length(x$features), length(x$impute))
  catf("impute.new.levels: %s", x$impute.new.levels)
  catf("recode.factor.levels: %s", x$recode.factor.levels)
  catf("dummy.type: %s", x$dummy.type)
}

#' Re-impute a data set
#'
#' This function accepts a data frame and a imputation description
#' as returned by \code{\link{impute}} to perform the following actions:
#' \enumerate{
#'   \item Restore dropped columns, setting them to \code{NA}
#'   \item Add dummy variables for columns as specified in \code{impute}
#'   \item Optionally check factors for new levels to treat them as \code{NA}s
#'   \item Reorder factor levels to ensure identical integer representation as
#'     before
#'   \item Impute missing values using previously collected data
#' }
#'
#' @param x [\code{data.frame}]\cr
#'   Object to reimpute. Currently only data frames are supported.
#' @param desc [\code{ImputationDesc}]\cr
#'   Imputation description as returned by \code{\link{impute}}.
#' @return Imputated \code{x}.
#' @family impute
#' @export
reimpute = function(x, desc) {
  UseMethod("reimpute")
}

#' @export
reimpute.list = function(x, desc) {
  reimpute.data.frame(as.data.frame(x), desc)
}

#' @export
reimpute.data.frame = function(x, desc) {
  assertClass(desc, classes = "ImputationDesc")
  x = as.list(x)

  # check for new columns
  new.cols = names(which(names(x) %nin% desc$cols))
  if (length(new.cols))
    stop("New columns (%s) found in data. Unable to impute.", collapse(new.cols))

  # restore dropped columns
  x[setdiff(desc$features, names(x))] = NA

  # calculate dummies as nums or factors (see option)
  dummy.cols = lapply(x[desc$dummies], is.na)
  names(dummy.cols) = sprintf("%s.dummy", desc$dummies)
  not.ok = which.first(names(dummy.cols) %in% names(x))
  if (length(not.ok))
    stopf("Dummy column '%s' already present in data", names(dummy.cols)[not.ok])
  dummy.cols = if (desc$dummy.type == "numeric")
    lapply(dummy.cols, as.numeric)
  else
    lapply(dummy.cols, factor, levels = c("FALSE", "TRUE"))

  # check for new levels and replace with NAs
  if (desc$impute.new.levels) {
    cols = names(desc$lvls)
    newlvls = Map(function(x, expected) setdiff(levels(x), expected),
      x = x[cols], expected = desc$lvls)
    newlvls = Filter(length, newlvls)
    if (length(newlvls))
      x[names(newlvls)] = Map(function(x, nl) droplevels(replace(x, x %in% nl, NA)),
        x = x[names(newlvls)], nl = newlvls)
  }

  # actually do the imputation
  cols = intersect(names(x), names(desc$impute))
  x[cols] = Map(
    function(xn, obj) do.call(obj$impute, c(list(data = x, target = desc$target, col = xn), obj$args)),
  xn = cols, obj = desc$impute[cols])

  # recode factor levels
  if (desc$recode.factor.levels) {
    cols = names(desc$lvls)
    x[cols] = Map(function(x, expected) {
      factor(as.character(x), levels = expected)
    }, x = x[cols], expected = desc$lvls)
  }

  x[names(dummy.cols)] = dummy.cols
  data.frame(x, stringsAsFactors = FALSE)
}
