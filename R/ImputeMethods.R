imputeConstFun = function(x, const) {
  if (is.factor(x) && const %nin% levels(x)) {
    levels(x) = c(levels(x), as.character(const))
  }
  replace(x, is.na(x), const)
}

#' @export
#' @param const [any]\cr
#'  Any object to use for imputation.
#' @rdname impute
imp.const = function(const) {
  force(const)
  setClasses(list(
    learn = function(...) const,
    impute = imputeConstFun
  ), "ImputationObject")
}

#' @export
#' @rdname impute
imp.median = function() {
  setClasses(list(
    learn = function(data, target, col, ...) median(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  ), "ImputationObject")
}

#' @export
#' @rdname impute
imp.mode = function() {
  setClasses(list(
    learn = function(data, target, col, ...) computeMode(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  ), "ImputationObject")
}

#' @export
#' @param multiplier [\code{numeric(1)}]\cr
#'  Numeric value to multiply the minimum or maximum.
#' @rdname impute
imp.min = function(multiplier=1) {
  force(multiplier)
  setClasses(list(
    learn = function(data, target, col, ...) multiplier*min(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  ), "ImputationObject")
}

#' @export
#' @rdname impute
imp.max = function(multiplier=1) {
  force(multiplier)
  setClasses(list(
    learn = function(data, target, col, ...) multiplier*max(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  ), "ImputationObject")
}

#' @export
#' @rdname impute
imp.normal = function() {
  setClasses(list(
    learn = function(data, target, col, ...)  {
      # FIXME handle cases where all(is.na(x)) == TRUE
      mu = mean(data[[col]], na.rm=TRUE)
      sd = sd(data[[col]], na.rm=TRUE)
      list(mu = mu, sd = sd)
    },
    impute = function(x, mu, sd) {
      ind = is.na(x)
      replace(x, ind, rnorm(sum(ind), mean=mu, sd=sd))
    }
  ), "ImputationObject")
}

#' @export
#' @param breaks [\code{numeric(1)}]\cr
#'  Number of breaks to use in \code{\link[graphics]{hist}}. If missing,
#'  defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids [\code{logical(1)}]\cr
#'  If \code{x} is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @rdname impute
imp.hist = function(breaks, use.mids=TRUE) {
  if (missing(breaks)) {
    breaks = "Sturges"
  } else {
    breaks = convertInteger(breaks)
    checkArg(breaks, "integer", len=1L, na.ok=FALSE)
  }
  checkArg(use.mids, "logical", len=1L, na.ok=FALSE)

  setClasses(list(
    learn = function(data, target, col, ...) {
      x = data[[col]]
      if (is.numeric(x)) {
        tmp = hist(x, breaks=breaks, plot=FALSE)
        if (use.mids)
          list(counts=tmp$counts, values=tmp$mids)
        else
          list(counts=tmp$counts, breaks=tmp$breaks)
      } else {
        tmp = table(x, useNA="no")
        values = names(tmp)
        if (is.logical(x))
          values = as.logical(x)
        list(counts=as.integer(tmp), values=values)
      }
    },
    impute = function(x, counts, values, breaks) {
      ind = which(is.na(x))
      if (missing(values)) {
        w = sample(seq_along(counts), length(ind), replace=TRUE, prob=counts)
        values = runif(length(ind), min=head(breaks, -1L)[w], max=tail(breaks, -1L)[w])
      } else {
        values = sample(values, length(ind), replace=TRUE, prob=counts)
      }
      replace(x, ind, values)
    }
  ), "ImputationObject")
}
