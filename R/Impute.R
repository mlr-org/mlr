# FIXME: is the target a string or a character vector? this is not explained!

# FIXME: Some of the special cool things need to be explained in a bit more detail

# FIXME: does it make sense to encoubter different features in reimpute set?
# if not, we shoukd check that.

# FIXME: we can enter crap in cols and classes without good check?

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
#'   Name of the column specifying the response.
#' @param classes [\code{named list}]\cr
#'   Named list containing imputation techniques for classes of columns.
#'   E.g. \code{list(numeric = imputeMedian())}.
#' @param cols [\code{named list}]\cr
#'   Named list containing names of the built-in imputation methods to impute missing values
#'   in the data column referenced by the list element's name. Overwrites imputation set via
#'   \code{classes}.
#' @param dummies [\code{character}]\cr
#'   Column names for which dummy variables (binary missing indicator) should be created.
#'   Default is \code{character(0)}.
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
#'   \item{data \code{data.frame}}{Imputed data.}
#'   \item{desc \code{ImputationDesc}}{Description object.}
#' @export
#' @examples
#' df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z=1:3)
#' imputed = impute(df, target=character(0), cols=list(x = 99, y = imputeMode()))
#' print(imputed$data)

# FIXME: seems buggy
# reimpute(data.frame(x=NA), imputed$desc)

impute = function(data, target, classes=list(), cols=list(), dummies=character(0L),
  impute.new.levels=TRUE, recode.factor.levels=TRUE) {

  checkArg(data, "data.frame")
  checkArg(target, "character", na.ok=FALSE)
  checkArg(classes, "list")
  checkArg(cols, "list")
  if (length(target)) {
    if (target %nin% names(data))
      stopf("Target column '%s' must be present in data", target)
    if (target %in% names(cols))
      stopf("Imputation of target column '%s' not possible", target)
    if (target %in% names(dummies))
      stopf("Dummy creation of target column '%s' not possible", target)
  }
  checkArg(impute.new.levels, "logical", len=1L, na.ok=FALSE)
  checkArg(recode.factor.levels, "logical", len=1L, na.ok=FALSE)

  features = setdiff(names(data), target)
  feature.classes = vapply(data[features], class, character(1L))

  # some elements need to be set to a non-harmful
  # setting for the first iteration of impute
  # those will be overwritten later
  desc = makeS3Obj("ImputationDesc",
    target = target,
    features = features,
    lvls = NULL,
    impute = namedList(features),
    dummies = dummies,
    impute.new.levels=FALSE,
    recode.factor.levels=FALSE
  )

  # handle classes -> insert into desc
  cl2 = feature.classes[feature.classes %in% names(classes) &
    names(feature.classes) %nin% names(cols)]
  desc$impute[names(cl2)] = classes[cl2]

  # handle cols -> insert into desc
  desc$impute[names(cols)] = cols

  # cleanup
  desc$impute = Filter(Negate(is.null), desc$impute)

  # learn and thereby transform to list(impute(...), args(...))
  desc$impute = Map(function(xn, x) {
    if (class(x)[1L] != "ImputeMethod")
      x = imputeConstant(x)
    list(
      impute = x$impute,
      # FIXME: can we avoid the data duplication here?
      args = do.call(x$learn, c(x$args, list(data=data, target=target, col=xn)))
    )
  }, xn=names(desc$impute), x=desc$impute)

  data = reimpute(data, desc)

  # store factor levels (this might include new levels created during imputation)
  ind = names(which(feature.classes == "factor"))
  desc$lvls = lapply(data[ind], levels)

  # set variables for consecutive imputes
  desc$recode.factor.levels = recode.factor.levels
  desc$impute.new.levels = impute.new.levels

  list(data=data, desc=desc)
}

#' @S3method print ImputationDesc
print.ImputationDesc = function(x, ...) {
  catf("Imputation description")
  catf("Target: %s", collapse(x$target))
  catf("Features: %i; Imputed: %i", length(x$features), length(x$impute))
  catf("impute.new.levels: %s", x$impute.new.levels)
  catf("recode.factor.levels: %s", x$recode.factor.levels)
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
#' @export
reimpute = function(x, desc) {
  UseMethod("reimpute")
}

#' @S3method reimpute list
reimpute.list = function(x, desc) {
  UseMethod("reimpute", as.data.frame(x))
}

#' @S3method reimpute data.frame
reimpute.data.frame = function(x, desc) {
  checkArg(desc, "ImputationDesc")
  x = as.list(x)

  # check for new columns
  new.cols = names(which(names(x) %nin% desc$cols))
  if (length(new.cols))
    stop("New columns (%s) found in data. Unable to impute.", collapse(new.cols))

  # restore dropped columns
  x[setdiff(desc$cols, names(x))] = NA

  # calculate dummies, these are boolean T / F masks, where NAs occured in input
  dummy.cols = lapply(x[desc$dummies], is.na)
  names(dummy.cols) = sprintf("%s.dummy", desc$dummies)


  # check for new levels and replace with NAs
  if (desc$impute.new.levels) {
    cols = names(desc$lvls)
    newlvls = Map(function(x, expected) setdiff(levels(x), expected),
      x=x[cols], expected=desc$lvls)
    newlvls = Filter(length, newlvls)
    if (length(newlvls))
      x[names(newlvls)] = Map(function(x, nl) droplevels(replace(x, x %in% nl, NA)),
        x=x[names(newlvls)], nl=newlvls)
  }

  # actually do the imputation
  cols = intersect(names(x), names(desc$impute))
  x[cols] = Map(
    function(xn, obj) do.call(obj$impute, c(list(data=x, target=desc$target, col=xn), obj$args)),
  xn=cols, obj=desc$impute[cols])

  # recode factor levels
  if (desc$recode.factor.levels) {
    cols = names(desc$lvls)
    x[cols] = Map(function(x, expected) {
      factor(as.character(x), levels=expected)
    }, x=x[cols], expected=desc$lvls)
  }

  x[names(dummy.cols)] = dummy.cols
  x = data.frame(x, stringsAsFactors=FALSE)
  # FIXME: remove assertion?
  stopifnot(is.data.frame(x))
  x
}

