#' Create an imputation method
#'
#' This is a constructor to create own imputation methods.
#'
#' @param learn [\code{function(data, target, col, ...)}]\cr
#'  Function to learn and extract information on column \code{col}
#'  out of data frame \code{data}. Argument \code{target} specifies
#'  the target column of the learning task.
#' @param impute [\code{function(data, target, col, ...)}]\cr
#'  Function to impute missing values in \code{col} using information
#'  returned by \code{learn} on the same column.
#'  The return value of \code{learn} is automatically passed to this function
#'  via \code{...}.
#' @param args [\code{list}]\cr
#'  Named list of arguments to pass to \code{learn} via \code{...}.
#' @export
makeImputeMethod = function(learn, impute, args=list()) {
  checkArg(learn, "function", formals=c("data", "target", "col"))
  checkArg(impute, "function", formals=c("data", "target", "col"))
  checkArg(args, "list")
  if (!isProperlyNamed(args))
    stop("All arguments must be properly named")
  setClasses(list(learn=learn, impute=impute, args=args), "ImputeMethod")
}

simpleImpute = function(data, target, col, const) {
  x = data[[col]]
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
    impute = simpleImpute,
    args = list(const=const)
  )
}

#' @export
#' @rdname impute
imputeMedian = function() {
  makeImputeMethod(
    learn = function(data, target, col) median(data[[col]], na.rm=TRUE),
    impute = simpleImpute
  )
}

#' @export
#' @rdname impute
imputeMode = function() {
  makeImputeMethod(
    learn = function(data, target, col) computeMode(data[[col]], na.rm=TRUE),
    impute = simpleImpute
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
    impute = simpleImpute,
    args = list(multiplier=multiplier)
  )
}

#' @export
#' @rdname impute
imputeMax = function(multiplier=1) {
  checkArg(multiplier, "numeric", len=1L, na.ok=FALSE)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) multiplier*max(data[[col]], na.rm=TRUE),
    impute = simpleImpute,
    args = list(multiplier=multiplier)
  )
}

#' @export
#' @param mu [\code{numeric(1)}]\cr
#'  Mean of normal distribution. If missing it will get estimated from the data.
#' @param sd [\code{numeric(1)}]\cr
#'  Standard deviation of normal distribution. If missing it will get estimated from the data.
#' @rdname impute
imputeUniform = function(min, max) {
  if (missing(min))
    min = NULL
  else
    checkArg(min, "numeric", len=1L, na.ok=FALSE)
  if (missing(max))
    max = NULL
  else
    checkArg(max, "numeric", len=1L, na.ok=FALSE)

  makeImputeMethod(
    learn = function(data, target, col, min, max)  {
      if (is.null(min))
        mu = min(data[[col]], na.rm=TRUE)
      if (is.null(max))
        sd = max(data[[col]], na.rm=TRUE)
      list(min=min, max=max)
    },
    impute = function(data, target, col, min, max) {
      x = data[[col]]
      ind = is.na(x)
      replace(x, ind, runif(sum(ind), min=min, max=max))
    },
    args = list(min=min, max=max)
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
    impute = function(data, target, col, mu, sd) {
      x = data[[col]]
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
    impute = function(data, target, col, counts, values, breaks) {
      x = data[[col]]
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

imputeLearner = function(learner, preimpute=list()) {
  checkArg(learner, "Learner")
  checkArg(preimpute, "list")
  if (!isProperlyNamed(preimpute))
    stop("All elements in preimpute must be properly named")

  makeImputeMethod(
    learn = function(data, target, col, learner, preimpute) {
      cl = class(learner)

      if (length(preimpute)) {
        x = do.call(impute, c(preimpute, list(data=data, target=target)))
        desc = x$desc
        data = x$data
      } else {
        desc = NULL
      }

      if ("RLearnerRegr" %in% cl) {
        task = makeRegrTask("impute", data=dropNamed(data, target), target=col)
      } else if ("RLearnerClassif" %in% cl) {
        task = makeClassifTask("impute", data=dropNamed(data, target), target=col)
      } else {
        stop("Unknown learner class for impute")
      }

      list(model=train(learner, task), desc=desc)
    },
    impute = function(data, target, col, model, desc) {
      if (!is.null(desc))
        data = reimpute(data, desc)

      predict(model, newdata=data)
    },
    args = list(learner=learner, preimpute=preimpute)
  )
}
