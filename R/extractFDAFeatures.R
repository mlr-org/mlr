#' @title Extract features from functional data.
#'
#' @description
#' Extract non-functional features from functional features using various methods.
#'
#' The function [extractFDAFeatures] performs the extraction for all functional features
#' via the methods specified in `feat.methods` and transforms all mentioned functional
#' (matrix) features into regular data.frame columns.
#' Additionally, a \dQuote{`extractFDAFeatDesc`} object
#' which contains learned coefficients and other helpful data for
#' re-extraction during the predict-phase is returned. This can be used with
#' [reextractFDAFeatures] in order to extract features during the prediction phase.
#'
#' @details
#' The description object contains these slots:
#'   * target [`character`]: See argument.
#'   * coln [`character`]: Colum names of data.
#'   * fd.cols [`character`]: Functional feature names.
#'   * extractFDAFeat [`list`]: Contains `feature.methods` and relevant
#'     parameters for reextraction.
#'
#' @param obj ([Task] | [data.frame])\cr
#'   Task or data.frame to extract functional features from.
#'   Must contain functional features as matrix columns.
#' @param target (`character(1)`)\cr
#'   Task target column. Only neccessary for data.frames
#'   Default is `character(0)`.
#' @param feat.methods (named [list])\cr
#'   List of functional features along with the desired methods for each functional feature.
#'   \dQuote{all} applies the [extractFDAFeatures] method to each
#'   functional feature.
#'   Names of `feat.methods` must match column names of functional features.
#'   Available feature extraction methods are available under family `fda_featextractor`.
#'   Specifying a functional feature multiple times with different extraction methods allows
#'   for the extraction of different features from the same functional.
#'   Default is [list()] which does nothing.
#' @param ... (any)\cr
#'   Further hyperparameters passed on to the `feat.methods` specified above.
#' @return ([list])
#'   \item{data|task ([data.frame] | [Task])}{Extracted features, same type as obj.}
#'   \item{desc (`extracFDAFeatDesc`)}{Description object. See description for details.}
#' @family fda
#' @export
#' @examples
#' df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
#' fdf = makeFunctionalData(df, fd.features = list(x1 = 1:4, x2 = 5:8), exclude.cols = "y")
#' task = makeClassifTask(data = fdf, target = "y")
#' extracted = extractFDAFeatures(task,
#'   feat.methods = list("x1" = extractFDAFourier(), "x2" = extractFDAWavelets(filter = "haar")))
#' print(extracted$task)
#' reextractFDAFeatures(task, extracted$desc)
extractFDAFeatures = function(obj, target = character(0L), feat.methods = list(), ...) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list(), ...) {

  fdf = getFunctionalFeatures(obj)
  assertDataFrame(fdf, min.cols = 1L)
  assertSubset(unique(names(feat.methods)), choices = c(names(fdf), "all"))
  assertCharacter(target)

  # If the same transform should be applied to all features, rep method and name accordingly
  # "all" needs to be first list name.
  all.fds = which(names(feat.methods) == "all")
  if (length(all.fds) > 0L) {
    methods = setNames(rep(feat.methods[all.fds], ncol(fdf)), rep(names(fdf), each = length(feat.methods[all.fds])))
    feat.methods[all.fds] = NULL
    feat.methods = c(feat.methods, methods)
  }

  desc = makeS3Obj("extractFDAFeatDesc",
    target = target,
    coln = colnames(obj),
    fd.cols = NULL,
    extractFDAFeat = namedList(names(feat.methods))
  )


  # cleanup empty list items
  feat.methods = Filter(Negate(is.null), feat.methods)

  # Treat method parameters:
  # Overwrite the par.vals from ... so it is set correctly during tuning
  feat.args = list(...)
  desc$extractFDAFeat = Map(function(x) {
    if (!is.null(x$par.set)) {
      # Only set relevant params
      feat.args = feat.args[names(feat.args) %in% getParamIds(x$par.set)]
    }
    if (length(feat.args) > 0) {
      # Overwrite args
      x$args = feat.args
    }
    return(x)
  }, feat.methods)

  # Subset fd.cols accordingly
  desc$fd.cols = names(desc$extractFDAFeat)
  # Apply function from x to all functional features and return as list of
  # lists for each functional feature.

  # The "learn" function only learns from the training data and returns the "extraction model".
  extracts = Map(function(x, fd.col) {
    reextract = x$reextract
    # Learn an "extraction model which is a trained algo for extracting from train data.
    extractor.vals = do.call(x$learn, c(x$args, list(data = obj, target = target, col = fd.col)))
    list("reextract" = reextract, "extractor.vals" = extractor.vals)
  }, x = desc$extractFDAFeat, fd.col = desc$fd.cols)

  # Append Info relevant for reextraction to desc
  desc$extractFDAFeat = extracts
  extracted = reextractFDAFeatures(obj, desc)

  list(data = extracted, desc = desc)
}

#' @export
extractFDAFeatures.Task = function(obj, target = character(0L), feat.methods = list(), ...) {

  stopifnot((hasFunctionalFeatures(obj)))

  data = getTaskData(obj, functionals.as = "matrix")
  target = getTaskTargetNames(obj)

  # Extract features from data
  extracted = extractFDAFeatures.data.frame(obj = data, target = target, feat.methods = feat.methods, ...)

  # And change data so it only contains non-functional features
  task = changeData(obj, extracted$data)
  list(task = task, desc = extracted$desc)
}


#' @export
print.extractFDAFeatDesc = function(x, ...) {
  catf("Extraction of features from functional data:")
  catf("Target: %s", collapse(x$target))
  catf("Remaining functional Features: %i after extraction on %i functional features",
    length(x$fd.cols), length(x$extractFDAFeat))
}


#' Re-extract features from a data set
#'
#' @description
#' This function accepts a data frame or a task and an extractFDAFeatDesc
#' (a FDA feature extraction description)
#' as returned by [extractFDAFeatures] to extract features
#' from previously unseen data.
#' @param obj ([Task] | [data.frame])\cr
#'   Task or data.frame to extract functional features from. Must contain functional features
#'   as matrix columns.
#' @param desc (`extractFDAFeatDesc`)\cr
#'   FDAFeature extraction description as returned by [extractFDAFeatures]
#' @param ... (any)\cr
#'  Further args passed on to methods.
#' @return [data.frame] or [Task] containing the extracted Features
#' @family extractFDAFeatures
#' @export
reextractFDAFeatures = function(obj, desc, ...) {
  UseMethod("reextractFDAFeatures")
}

#' @export
reextractFDAFeatures.data.frame = function(obj, desc, ...) {

  assertClass(desc, classes = "extractFDAFeatDesc")

  # check for new columns
  new.cols = names(which(names(obj) %nin% desc$coln))
  if (length(new.cols)) {
    stop("New columns (%s) found in data. Unable to extract.", collapse(new.cols))
  }

  # reextract features using reextractDescription and return
  reextract = Map(
    function(xn, x, fd.col) {
      df = do.call(x$reextract, c(list(data = obj, target = desc$target, col = fd.col, vals = x$extractor.vals)))
      # Check returned vals
      if (!is.data.frame(df)) {
        stop("feat.method needs to return a data.frame with one row per observation in the original data.")
      } else if (nrow(df) != nrow(obj)) {
        stop("feat.method needs to return a data.frame with one row per observation in the original data and equal nrow per column.")
      }
      return(df)
    },
    xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.col = desc$fd.cols)

  # cbind resulting columns. Use data.table to ensure proper naming.
  df = as.data.frame(do.call(cbind, lapply(reextract, setDT)))

  # Reappend target and non-functional features
  keep.cols = setdiff(colnames(obj), desc$fd.cols)
  data = cbind(df, obj[keep.cols])
  return(data)
}


#' @export
reextractFDAFeatures.Task = function(obj, desc, ...) {
  # get data and pass to extractor
  df = getTaskData(obj, functionals.as = "matrix")
  extracted = reextractFDAFeatures.data.frame(df, desc)
  # Change data of task and return task
  changeData(obj, extracted)
}
