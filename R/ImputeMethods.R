# FIXME: From a design point of view, using the task here as input argument
# might be more extensible for us later and
# convenient for the user.
# For us, we can use polymorphism, and for the user
# the task contains a lot of helpful info.
# But I do not want to revamp the structure now

#FIXME: what happens in cases when a whole column is NA?

#' Create a custom imputation method.
#'
#' This is a constructor to create your own imputation methods.
#' @param learn [\code{function(data, target, col, ...)}]\cr
#'   Function to learn and extract information on column \code{col}
#'   out of data frame \code{data}. Argument \code{target} specifies
#'   the target column of the learning task.
#'   The function has to return a named list of values.
#' @param impute [\code{function(data, target, col, ...)}]\cr
#'   Function to impute missing values in \code{col} using information
#'   returned by \code{learn} on the same column.
#'   All list elements of the return values o \code{learn}
#'   are passed to this function into \code{...}.
#' @param args [\code{list}]\cr
#'   Named list of arguments to pass to \code{learn} via \code{...}.
#' @export
makeImputeMethod = function(learn, impute, args = list()) {
  checkArg(learn, "function", formals = c("data", "target", "col"))
  checkArg(impute, "function", formals = c("data", "target", "col"))
  checkArg(args, "list")
  if (!isProperlyNamed(args))
    stop("All arguments must be properly named")
  setClasses(list(learn = learn, impute = impute, args = args), "ImputeMethod")
}

# helper function to impute missings of a col to const val
simpleImpute = function(data, target, col, const) {
  x = data[[col]]
  if (is.factor(x) && const %nin% levels(x)) {
    levels(x) = c(levels(x), as.character(const))
  }
  replace(x, is.na(x), const)
}


#' Built in imputation methods
#' The built-ins are:
#' \itemize{
#'   \item \code{imputeConstant(const)} for imputation using a constant value,
#'   \item \code{imputeMedian()} for imputation using the median,
#'   \item \code{imputeMode()} for imputation using the mode,
#'   \item \code{imputeMin(multiplier)} for imputation using the minimum,
#'   \item \code{imputeMax(multiplier)} for imputation using the maximum,
#'   \item \code{imputeNormal(mean, sd)} for imputation using normally
#'     distributed random values. Mean and standard deviation will be calculated
#'     from the data if not provided.
#'   \item \code{imputeHist(breaks, use.mids)} for imputation using random values
#'     with probabilities calculated using \code{table} or \code{hist}.
#'   \item \code{imputeLearner(learner, preimpute)} for imputations using the response
#'     of a classification or regression learner.
#' }
#' @name imputations
#' @rdname imputations
NULL

#' @export
#' @param const [any]\cr
#'  Constant valued use for imputation.
#' @rdname imputations
imputeConstant = function(const) {
  makeImputeMethod(
    learn = function(data, target, col, const) const,
    impute = simpleImpute,
    args = list(const = const)
  )
}

#' @export
#' @rdname imputations
imputeMedian = function() {
  makeImputeMethod(
    learn = function(data, target, col) median(data[[col]], na.rm = TRUE),
    impute = simpleImpute
  )
}

#' @export
#' @rdname imputations
imputeMean = function() {
  makeImputeMethod(
    learn = function(data, target, col) mean(data[[col]], na.rm = TRUE),
    impute = simpleImpute
  )
}

#' @export
#' @rdname imputations
imputeMode = function() {
  makeImputeMethod(
    learn = function(data, target, col) computeMode(data[[col]], na.rm = TRUE),
    impute = simpleImpute
  )
}

#' @export
#' @param multiplier [\code{numeric(1)}]\cr
#'   Value that stored minimum or maximum is multiplied with when imputation is done.
#' @rdname imputations
imputeMin = function(multiplier = 1) {
  checkArg(multiplier, "numeric", len = 1L, na.ok = FALSE)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) multiplier*min(data[[col]], na.rm = TRUE),
    impute = simpleImpute,
    args = list(multiplier = multiplier)
  )
}

#' @export
#' @rdname imputations
imputeMax = function(multiplier = 1) {
  checkArg(multiplier, "numeric", len = 1L, na.ok = FALSE)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) multiplier*max(data[[col]], na.rm = TRUE),
    impute = simpleImpute,
    args = list(multiplier = multiplier)
  )
}

#' @export
#' @param min [\code{numeric(1)}]\cr
#'   Lower bound for uniform distribution. If missing it will be estimated from the data.
#' @param max [\code{numeric(1)}]\cr
#'   Upper bound for uniform distribution. If missing it will be estimated from the data.
#' @rdname imputations
imputeUniform = function(min, max) {
  if (missing(min))
    min = NULL
  else
    checkArg(min, "numeric", len = 1L, na.ok = FALSE)
  if (missing(max))
    max = NULL
  else
    checkArg(max, "numeric", len = 1L, na.ok = FALSE)

  makeImputeMethod(
    learn = function(data, target, col, min, max)  {
      if (is.null(min))
        min = min(data[[col]], na.rm = TRUE)
      if (is.null(max))
        max = max(data[[col]], na.rm = TRUE)
      list(min = min, max = max)
    },
    impute = function(data, target, col, min, max) {
      x = data[[col]]
      ind = is.na(x)
      replace(x, ind, runif(sum(ind), min = min, max = max))
    },
    args = list(min = min, max = max)
  )
}

#' @export
#' @param mu [\code{numeric(1)}]\cr
#'   Mean of normal distribution. If missing it will be estimated from the data.
#' @param sd [\code{numeric(1)}]\cr
#'   Standard deviation of normal distribution. If missing it will be estimated from the data.
#' @rdname imputations
imputeNormal = function(mu, sd) {
  if (missing(mu))
    mu = NULL
  else
    checkArg(mu, "numeric", len = 1L, na.ok = FALSE)
  if (missing(sd))
    sd = NULL
  else
    checkArg(sd, "numeric", len = 1L, na.ok = FALSE)

  makeImputeMethod(
    learn = function(data, target, col, mu, sd)  {
      if (is.null(mu))
        mu = mean(data[[col]], na.rm = TRUE)
      if (is.null(sd))
        sd = sd(data[[col]], na.rm = TRUE)
      list(mu = mu, sd = sd)
    },
    impute = function(data, target, col, mu, sd) {
      x = data[[col]]
      ind = is.na(x)
      replace(x, ind, rnorm(sum(ind), mean = mu, sd = sd))
    },
    args = list(mu = mu, sd = sd)
  )
}

#' @export
#' @param breaks [\code{numeric(1)}]\cr
#'  Number of breaks to use in \code{\link[graphics]{hist}}. If missing,
#'  defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids [\code{logical(1)}]\cr
#'  If \code{x} is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @rdname imputations
imputeHist = function(breaks, use.mids = TRUE) {
  if (missing(breaks)) {
    breaks = "Sturges"
  } else {
    breaks = convertInteger(breaks)
    checkArg(breaks, "integer", len = 1L, na.ok = FALSE)
  }
  checkArg(use.mids, "logical", len = 1L, na.ok = FALSE)

  makeImputeMethod(

    learn = function(data, target, col, breaks, use.mids) {
      x = data[[col]]
      # numeric / integer feature
      if (is.numeric(x)) {
        tmp = hist(x, breaks = breaks, plot = FALSE)
        if (use.mids)
          return(list(counts = tmp$counts, values = tmp$mids))
        else
          return(list(counts = tmp$counts, breaks = tmp$breaks))
      # factor or logical feature
      } else {
        tmp = table(x, useNA = "no")
        values = names(tmp)
        if (is.logical(x))
          values = as.logical(x)
        return(list(counts = as.integer(tmp), values = values))
      }
    },

    impute = function(data, target, col, counts, values, breaks) {
      x = data[[col]]
      ind = which(is.na(x))
      if (missing(values)) {
        w = sample(seq_along(counts), length(ind), replace = TRUE, prob = counts)
        values = runif(length(ind), min = head(breaks, -1L)[w], max = tail(breaks, -1L)[w])
      } else {
        values = sample(values, length(ind), replace = TRUE, prob = counts)
      }
      replace(x, ind, values)
    },
    args = list(breaks = breaks, use.mids = use.mids)
  )
}

#' @rdname imputations
#' @param learner [\code{Learner}]\cr
#'  Classification or regression learner. Its predictions will be used for imputations.
#'  Note that the target column is not available for this operation.
#' @param preimpute [\code{list}]\cr
#'  Arguments for inner call to \code{\link{impute}} in scenarios where
#'  \code{learner} itself cannot handle missing values.
#'  Default is \code{list()}.
#' @export
imputeLearner = function(learner, preimpute = list()) {
  # FIXME: this function needs some love
  checkArg(learner, "Learner")
  checkArg(preimpute, "list")
  if (!isProperlyNamed(preimpute))
    stop("All elements in preimpute must be properly named")

  makeImputeMethod(
    learn = function(data, target, col, learner, preimpute) {
      cl = class(learner)

      if (length(preimpute)) {
        x = do.call(impute, c(preimpute, list(data = data, target = target)))
        desc = x$desc
        data = x$data
      } else {
        desc = NULL
      }

      if ("RLearnerRegr" %in% cl) {
        task = makeRegrTask("impute", data = dropNamed(data, target), target = col)
      } else if ("RLearnerClassif" %in% cl) {
        task = makeClassifTask("impute", data = dropNamed(data, target), target = col)
      } else {
        stop("Unknown learner class for impute")
      }

      list(model = train(learner, task), desc = desc)
    },
    impute = function(data, target, col, model, desc) {
      if (!is.null(desc))
        data = reimpute(data, desc)

      predict(model, newdata = data)
    },
    args = list(learner = learner, preimpute = preimpute)
  )
}
