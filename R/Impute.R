#' Impute and re-impute data
#'
#' These functions provide some convenience for imputation of missing values.
#' A noteworthy feature is given by the possibility to re-impute a data set
#' in the same way as learned previously which especially becomes handy in
#' combination with resampling strategies.
#'
#' The function \code{impute} performs the imputations on a data set and returns,
#' alongside with the imputed data set, an \dQuote{ImputationDesc} object which
#' then can be passed together with a new data set to \code{\link{reimpute}}.
#'
#' The imputation techniques can be specified for certain features or using the
#' feature classes, see parameters. You can either provide an arbitrary object
#' use a built-in imputation method or create one yourself using ...
#' FIXME we should provide a constructor and document it.
#' The built-ins  are:
#' \itemize{
#'   \item \code{imp.const(const)} for imputation using a constant value,
#'   \item \code{imp.median()} for imputation using the median,
#'   \item \code{imp.mode()} for imputation using the mode,
#'   \item \code{imp.min(multiplier)} for imputation using the minimum,
#'   \item \code{imp.max(multiplier)} for imputation using the maximum,
#'   \item \code{imp.normal()} for imputation using normally distributed random
#'     values with mean and variance estimated from the data,
#'   \item \code{imp.hist(breaks, use.mids)} for imputation using random values
#'     with probabilities calculated using \code{table} or \code{hist}.
#' }
#'
#' @param data [\code{data.frame}]\cr
#'  Input data.
#' @param target [\code{character}]\cr
#'  Name of the column specifying the response.
#' @param classes [\code{named list}]\cr
#'  Named list containing imputation techniques for classes of columns. E.g. \code{list(integer = 0)}.
#' @param cols [\code{named list}]\cr
#'  Named list containing names of the built-in imputation methods to impute missing values
#'  in the data column referenced by the list element's name. Overwrites imputation set via
#'  \code{classes}.
#' @param dummies [\code{character()}]\cr
#'  Set of column names for which dummy variables (binary missing indicator) should be created.
#'  Default is \code{character(0)}.
#' @param impute.newlevels [\code{logical(1)}]\cr
#'  Impute new levels in factors the same way as missing values? Default is \code{TRUE}.
#' @return \code{list} with two named elements \dQuote{data} and \dQuote{desc}.
#' @export
#' @examples
#'  df = data.frame(x = c(1, 1, NA), y = factor(c("a", "a", "b")), z=1:3)
#'  imputed = impute(df, target=character(0), cols=list(x = 99, y = imp.mode()))
#'  print(imputed$data)
#'  reimpute(data.frame(x=NA), imputed$desc)
impute = function(data, target, classes = list(), cols = list(), dummies=character(0L), impute.newlevels=TRUE) {
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
  checkArg(impute.newlevels, "logical", len=1L, na.ok=FALSE)

  cn = setdiff(names(data), target)
  cl = vapply(data[cn], class, character(1L))
  desc = setClasses(list(
    impute = namedList(cn),
    dummies = dummies,
    lvls = NULL,
    impute.newlevels=impute.newlevels,
    cols = cn
  ), "ImputationDesc")

  # handle classes -> insert into desc
  cl2 = cl[cl %in% names(classes) & names(cl) %nin% names(cols)]
  desc$impute[names(cl2)] = classes[cl2]

  # handle cols -> insert into desc
  desc$impute[names(cols)] = cols

  # cleanup
  desc$impute = Filter(Negate(is.null), desc$impute)

  # learn and thereby transform to list(impute(...), args(...))
  desc$impute = Map(function(xn, x) {
    if (class(x)[1L] != "ImputeMethod")
      x = imp.const(x)
    list(impute = x$impute, args = x$learn(data, target, xn))
  }, names(desc$impute), desc$impute)

  data = reimpute(data, desc, .initial=TRUE)

  # store factor levels (this might include new levels created during imputation)
  ind = names(which(cl == "factor"))
  desc$lvls = lapply(data[ind], levels)

  # return
  list(data = data, desc=desc)
}


#' Re-impute a data set
#'
#' This function accepts a data set and a imputation description
#' returned by \code{\link{impute}} to perform the following actions:
#' \enumerate{
#'   \item Restore dropped columns, setting them to \code{NA}
#'   \item Add dummy variables for columns as specified in \code{impute}
#'   \item Optionally check factors for new levels to treat them as \code{NA}s
#'   \item Reorder factor levels to ensure the same integer representation
#'   \item Impute missing values using data collected on original data set
#' }
#'
#' @param x [\code{data.frame}]\cr
#'   Object to reimpute. Currently only data frames are supported.
#' @param desc [\code{ImputationDesc}]\cr
#'   Imputation description as returned by \code{\link{impute}}.
#' @param .initial [\code{logical(1)}]\cr
#'   Internally used to disable some imputations for the first run of impute.
#' @return Imputated \code{x}.
#' @export
reimpute = function(x, desc, .initial=FALSE) {
  UseMethod("reimpute")
}

#' @S3method reimpute data.frame
reimpute.data.frame = function(x, desc, .initial=FALSE) {
  checkArg(desc, "ImputationDesc")
  checkArg(.initial, "logical", len=1L, na.ok=FALSE)

  if (!.initial) {
    new.cols = names(which(names(x) %nin% desc$cols))
    if (length(new.cols))
      stop("New columns (%s) found in data. Unable to impute.", collapse(new.cols))

    # restore dropped columns
    x[setdiff(desc$cols, names(x))] = NA
  }

  # store dummies
  dummies = lapply(x[desc$dummies], is.na)
  names(dummies) = sprintf("%s.dummy", names(dummies))

  if (!.initial) {
    # check for new levels and replace with NAs
    if (desc$impute.newlevels) {
      cols = names(desc$lvls)
      newlvls = Map(function(x, expected) setdiff(levels(x), expected), x=x[cols], expected=desc$lvls)
      newlvls = Filter(length, newlvls)
      if (length(newlvls))
        x[names(newlvls)] = Map(function(x, nl) droplevels(replace(x, x %in% nl, NA)), x=x[names(newlvls)], nl=newlvls)
    }
  }

  # impute
  cols = intersect(names(x), names(desc$impute))
  x[cols] = Map(function(x, obj) do.call(obj$impute, c(list(x=x), obj$args)), x=x[cols], obj=desc$impute[cols])

  if (!.initial) {
    # resort factor levels
    cols = names(desc$lvls)
    x[cols] = Map(function(x, expected) {
      factor(as.character(x), levels=expected)
    }, x=x[cols], expected=desc$lvls)
  }

  data.frame(c(x, dummies), stringsAsFactors=FALSE)
}
