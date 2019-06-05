# FIXME: From a design point of view, using the task here as input argument
# might be more extensible for us later and
# convenient for the user.
# For us, we can use polymorphism, and for the user
# the task contains a lot of helpful info.
# But I do not want to revamp the structure now

#' Create a custom imputation method.
#'
#' This is a constructor to create your own imputation methods.
#' @param learn (`function(data, target, col, ...)`)\cr
#'   Function to learn and extract information on column `col`
#'   out of data frame `data`. Argument `target` specifies
#'   the target column of the learning task.
#'   The function has to return a named list of values.
#' @param impute (`function(data, target, col, ...)`)\cr
#'   Function to impute missing values in `col` using information
#'   returned by `learn` on the same column.
#'   All list elements of the return values o `learn`
#'   are passed to this function into `...`.
#' @param args ([list])\cr
#'   Named list of arguments to pass to `learn` via `...`.
#' @family impute
#' @export
makeImputeMethod = function(learn, impute, args = list()) {
  assertFunction(learn, args = c("data", "target", "col"))
  assertFunction(impute, args = c("data", "target", "col"))
  assertList(args, names = "named")
  setClasses(list(learn = learn, impute = impute, args = args), "ImputeMethod")
}

# helper function to impute missings of a col to const val
simpleImpute = function(data, target, col, const) {
  if (is.na(const)) {
    stopf("Error imputing column '%s'. Maybe all input data was missing?", col)
  }
  x = data[[col]]

  # cast logicals to factor if required (#1522)
  if (is.logical(x) && !is.logical(const)) {
    x = as.factor(x)
  }
  if (is.factor(x) && const %nin% levels(x)) {
    levels(x) = c(levels(x), as.character(const))
  }
  replace(x, is.na(x), const)
}


#' Built-in imputation methods.
#'
#' The built-ins are:
#' \itemize{
#'   \item `imputeConstant(const)` for imputation using a constant value,
#'   \item `imputeMedian()` for imputation using the median,
#'   \item `imputeMode()` for imputation using the mode,
#'   \item `imputeMin(multiplier)` for imputing constant values shifted below the minimum
#'     using `min(x) - multiplier * diff(range(x))`,
#'   \item `imputeMax(multiplier)` for imputing constant values shifted above the maximum
#'     using `max(x) + multiplier * diff(range(x))`,
#'   \item `imputeNormal(mean, sd)` for imputation using normally
#'     distributed random values. Mean and standard deviation will be calculated
#'     from the data if not provided.
#'   \item `imputeHist(breaks, use.mids)` for imputation using random values
#'     with probabilities calculated using `table` or `hist`.
#'   \item `imputeLearner(learner, features = NULL)` for imputations using the response
#'     of a classification or regression learner.
#' }
#' @name imputations
#' @rdname imputations
#' @family impute
NULL

#' @export
#' @param const (any)\cr
#'  Constant valued use for imputation.
#' @rdname imputations
imputeConstant = function(const) {
  assertVector(const, len = 1L, any.missing = FALSE)
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
#' @param multiplier (`numeric(1)`)\cr
#'   Value that stored minimum or maximum is multiplied with when imputation is done.
#' @rdname imputations
imputeMin = function(multiplier = 1) {
  assertNumber(multiplier)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) {
      r = range(data[[col]], na.rm = TRUE)
      r[1L] - multiplier * diff(r)
    },
    impute = simpleImpute,
    args = list(multiplier = multiplier)
  )
}

#' @export
#' @rdname imputations
imputeMax = function(multiplier = 1) {
  assertNumber(multiplier)
  makeImputeMethod(
    learn = function(data, target, col, multiplier) {
      r = range(data[[col]], na.rm = TRUE)
      r[2L] + multiplier * diff(r)
    },
    impute = simpleImpute,
    args = list(multiplier = multiplier)
  )
}

#' @export
#' @param min (`numeric(1)`)\cr
#'   Lower bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @param max (`numeric(1)`)\cr
#'   Upper bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @rdname imputations
imputeUniform = function(min = NA_real_, max = NA_real_) {
  assertNumber(min, na.ok = TRUE)
  assertNumber(max, na.ok = TRUE)
  makeImputeMethod(
    learn = function(data, target, col, min, max) {
      if (is.na(min)) {
        min = min(data[[col]], na.rm = TRUE)
        if (is.na(min)) {
          stop("All values are missing. Unable to calculate minimum.")
        }
      }
      if (is.na(max)) {
        max = max(data[[col]], na.rm = TRUE)
        if (is.na(max)) {
          stop("All values are missing. Unable to calculate maximum.")
        }
      }
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
#' @param mu (`numeric(1)`)\cr
#'   Mean of normal distribution. If missing it will be estimated from the data.
#' @param sd (`numeric(1)`)\cr
#'   Standard deviation of normal distribution. If missing it will be estimated from the data.
#' @rdname imputations
imputeNormal = function(mu = NA_real_, sd = NA_real_) {
  assertNumber(mu, na.ok = TRUE)
  assertNumber(sd, na.ok = TRUE)

  makeImputeMethod(
    learn = function(data, target, col, mu, sd) {
      if (is.na(mu)) {
        mu = mean(data[[col]], na.rm = TRUE)
        if (is.na(mu)) {
          stop("All values missing. Unable to calculate mean.")
        }
      }
      if (is.na(sd)) {
        sd = sd(data[[col]], na.rm = TRUE)
        if (is.na(sd)) {
          stop("All values missing. Unable to calculate sd.")
        }
      }
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
#' @param breaks (`numeric(1)`)\cr
#'  Number of breaks to use in [graphics::hist]. If missing,
#'  defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids (`logical(1)`)\cr
#'  If `x` is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @rdname imputations
imputeHist = function(breaks, use.mids = TRUE) {
  if (missing(breaks)) {
    breaks = "Sturges"
  }
  if (!identical(breaks, "Sturges")) {
    breaks = asCount(breaks)
  }
  assertFlag(use.mids)

  makeImputeMethod(

    learn = function(data, target, col, breaks, use.mids) {
      x = data[[col]]
      if (all(is.na(x))) {
        stop("All values missing. Unable to impute with Hist.")
      }
      if (is.numeric(x)) {
        tmp = hist(x, breaks = breaks, plot = FALSE)
        if (use.mids) {
          return(list(counts = tmp$counts, values = tmp$mids))
        } else {
          return(list(counts = tmp$counts, breaks = tmp$breaks))
        }
      } else { # factor or logical feature
        tmp = table(x, useNA = "no")
        values = names(tmp)
        if (is.logical(x)) {
          values = as.logical(x)
        }
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

#' @param learner ([Learner] | `character(1)`)\cr
#'  Supervised learner. Its predictions will be used for imputations.
#'  If you pass a string the learner will be created via [makeLearner].
#'  Note that the target column is not available for this operation.
#' @param features ([character])\cr
#'  Features to use in `learner` for prediction.
#'  Default is `NULL` which uses all available features except the target column
#'  of the original task.
#' @rdname imputations
#' @export
imputeLearner = function(learner, features = NULL) {
  learner = checkLearner(learner)
  if (!is.null(features)) {
    assertCharacter(features, any.missing = FALSE)
  }

  makeImputeMethod(
    learn = function(data, target, col, learner, features) {

      constructor = getTaskConstructorForLearner(learner)
      if (is.null(features)) {
        features = setdiff(names(data), target)
      } else {
        not.ok = which(features %nin% names(data))
        if (length(not.ok)) {
          stopf("Features for imputation not found in data: '%s'", collapse(features[not.ok]))
        }
        not.ok = which.first(target %in% features)
        if (length(not.ok)) {
          stopf("Target column used as feature for imputation: '%s'", target[not.ok])
        }
        if (col %nin% features) {
          features = c(col, features)
        }
      }
      # features used for imputation might have NAs, but the learner might not support that
      # we need an extra check, otherwise this might not get noticed by checkLearnerBeforeTrain because
      # we remove observations with NAs in column col before generating the task
      impute.feats = setdiff(features, col)
      if (anyMissing(data[impute.feats]) && !hasLearnerProperties(learner, "missings")) {
        has.na = vlapply(data[impute.feats], anyMissing)
        wrong.feats = clipString(collapse(colnames(data[impute.feats])[has.na], ", "), 50L)
        stopf("Feature(s) '%s' used for imputation has/have missing values, but learner '%s' does not support that!", wrong.feats, learner$id)
      }
      # remove all observations with missing values in column col (which is the target in the imputation task)
      ind = !is.na(data[[col]])
      task = constructor("impute", data = subset(data, subset = ind, select = features), target = col,
        check.data = TRUE, fixup.data = "quiet")
      list(model = train(learner, task), features = features)
    },

    impute = function(data, target, col, model, features) {
      x = data[[col]]
      ind = is.na(x)
      # if no NAs are present in data, we always return it unchanged
      if (all(!ind)) {
        return(x)
      }
      newdata = as.data.frame(data)[ind, features, drop = FALSE]
      p = predict(model, newdata = newdata)$data$response
      replace(x, ind, p)
    },
    args = list(learner = learner, features = features)
  )
}
