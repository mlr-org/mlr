makeImputeMethod = function(learn, impute, args=list()) {
  checkArg(learn, "function", formals=c("data", "target", "col"))
  checkArg(impute, "function", formals="x")
  checkArg(args, "list")
  if (!isProperlyNamed(args))
    stop("All arguments must be properly named")
  setClasses(list(learn=learn, impute=impute, args=args), "ImputeMethod")
}

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
imputeConstant = function(const) {
  makeImputeMethod(
    learn = function(data, target, col, const) const,
    impute = imputeConstFun,
    args = list(const=const)
  )
}

#' @export
#' @rdname impute
imputeMedian = function() {
  makeImputeMethod(
    learn = function(data, target, col) median(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  )
}

#' @export
#' @rdname impute
imputeMode = function() {
  makeImputeMethod(
    learn = function(data, target, col) computeMode(data[[col]], na.rm=TRUE),
    impute = imputeConstFun
  )
}

#' @export
#' @param multiplier [\code{numeric(1)}]\cr
#'  Numeric value to multiply the minimum or maximum.
#' @rdname impute
imputeMin = function(multiplier=1) {
  checkArg(multiplier, "numeric", len=1L, na.ok=FALSE)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) multiplier*min(data[[col]], na.rm=TRUE),
    impute = imputeConstFun,
    args = list(multiplier=multiplier)
  )
}

#' @export
#' @rdname impute
imputeMax = function(multiplier=1) {
  checkArg(multiplier, "numeric", len=1L, na.ok=FALSE)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) multiplier*max(data[[col]], na.rm=TRUE),
    impute = imputeConstFun,
    args = list(multiplier=multiplier)
  )
}

#' @export
#' @param mu [\code{numeric(1)}]\cr
#'  Mean of normal distribution. If missing it will get estimated from the data.
#' @param sd [\code{numeric(1)}]\cr
#'  Standard deviation of normal distribution. If missing it will get estimated from the data.
#' @rdname impute
imputeNormal = function(mu, sd) {
  if (missing(mu))
    mu = NULL
  else
    checkArg(mu, "numeric", len=1L, na.ok=FALSE)
  if (missing(sd))
    sd = NULL
  else
    checkArg(sd, "numeric", len=1L, na.ok=FALSE)

  makeImputeMethod(
    learn = function(data, target, col, mu, sd)  {
      if (is.null(mu))
        mu = mean(data[[col]], na.rm=TRUE)
      if (is.null(sd))
        sd = sd(data[[col]], na.rm=TRUE)
      list(mu=mu, sd=sd)
    },
    impute = function(x, mu, sd) {
      ind = is.na(x)
      replace(x, ind, rnorm(sum(ind), mean=mu, sd=sd))
    },
    args = list(mu=mu, sd=sd)
  )
}

#' @export
#' @param breaks [\code{numeric(1)}]\cr
#'  Number of breaks to use in \code{\link[graphics]{hist}}. If missing,
#'  defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids [\code{logical(1)}]\cr
#'  If \code{x} is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @rdname impute
imputeHist = function(breaks, use.mids=TRUE) {
  if (missing(breaks)) {
    breaks = "Sturges"
  } else {
    breaks = convertInteger(breaks)
    checkArg(breaks, "integer", len=1L, na.ok=FALSE)
  }
  checkArg(use.mids, "logical", len=1L, na.ok=FALSE)

  makeImputeMethod(
    learn = function(data, target, col, breaks, use.mids) {
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
    },
    args = list(breaks=breaks, use.mids=use.mids)
  )
}
