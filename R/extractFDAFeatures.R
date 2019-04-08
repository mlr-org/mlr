#' @title Extract features from functional data.
#'
#' @description
#' Extract non-functional features from functional features using various methods.
#' The function [extractFDAFeatures] performs the extraction for all functional features
#' via the methods specified in `feat.methods` and transforms all mentioned functional
#' matrix features into regular data.frame columns.
#' Additionally, a \dQuote{`extractFDAFeatDesc`} object
#' which contains learned coefficients and other helpful data for
#' extraction during the predict-phase is returned. This can be used with
#' [reextractFDAFeatures] in order to extract features during the prediction phase.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [character]}{See argument.}
#'   \item{coln [character]}{colum names of data.}
#'   \item{fd.cols [character]}{Functional feature names.}
#'   \item{extractFDAFeat [list]}{Contains `feature.methods` and relevant
#'   parameters for reextraction}.
#' }
#'
#' @param obj ([Task] | [data.frame])\cr
#'   Task or data.frame to extract functional features from.
#'   Must contain functional features as matrix columns.
#' @param target ([character])\cr
#'   Task target column. Only neccessary for data.frames
#'   Default is `character(0)`.
#' @param feat.methods (named [list])\cr
#'   List of functional features along with the desired methods for each functional feature.
#'   \dQuote{all} applies the [extractFDAFeatures] method to each
#'   functional feature.
#'   Names of `feat.methods` must match column names of functional features.
#'   Available feature extraction methods are available under family `fda_featextractor`.
#'   Default is [list] which does nothing.
#' @return ([list])
#'   \item{data ([data.frame] | [Task])}{Extracted features, returns a data.frame when
#'   given a [data.frame] and a Task when given a Task.}
#'   \item{desc (`extractFDAFeatDesc`)}{Description object. See description for details.}
#' @family fda
#' @export


# FIXME: this does not run because of a problem in extractFDAWavelets
# @examples
# df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
# fdf = makeFunctionalData(df, fd.features = list(x1 = 1:4, x2=5:8), exclude.cols = "y")
# task = makeClassifTask(data = fdf, target = "y")
# extracted = extractFDAFeatures(task,
# feat.methods = list("x1" = extractFDAFourier(), "x2" = extractFDAWavelets()))
# print(extracted$task)
# reextractFDAFeatures(task, extracted$desc)

extractFDAFeatures = function(obj, target = character(0L), feat.methods = list()) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list()) {

  fdf = getFunctionalFeatures(obj)
  assertDataFrame(fdf, min.cols = 1L)
  assertSubset(unique(names(feat.methods)), choices = c(names(fdf),  "all"))
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

  desc$extractFDAFeat = feat.methods
  # cleanup empty list items
  desc$extractFDAFeat = Filter(Negate(is.null), desc$extractFDAFeat)
  # Subset fd.cols accordingly
  desc$fd.cols = names(desc$extractFDAFeat)
  # Apply function from x to all functional features and return as list of
  # lists for each functional feature.
  extracts = Map(function(x, fd.col) {
    list(
      # feats are the extracted features
      feats = do.call(x$learn, c(x$args, list(data = obj, target = target, col = fd.col))),
      args = x$args, # Args passed to x$reextract
      reextract = x$reextract  # pass on reextraction learner for extraction in prediction
    )
  }, x = desc$extractFDAFeat, fd.col = desc$fd.cols)

  # Append Info relevant for reextraction to desc
  desc$extractFDAFeat = lapply(extracts, function(x) {c(x["args"], x["reextract"])})

  # Extract feats for every functional feature and cbind to data.frame
  vals = extractSubList(extracts, "feats", simplify = FALSE)

  if (!all(vlapply(vals, is.data.frame))) {
    stop("feat.method needs to return a data.frame with one row per observation in the original data.")
  } else if (any(unique(vnapply(vals, nrow)) != nrow(obj))) {
    stop("feat.method needs to return a data.frame with one row per observation in the original data and equal nrow per column.")
  }
  # cbind resulting columns. Use data.table to ensure proper naming.
  df = as.data.frame(do.call(cbind, lapply(vals, setDT)))

  # Reappend target and non-functional features
  keep.cols = setdiff(desc$coln, desc$fd.cols)
  data = cbind(df, obj[, keep.cols, drop = FALSE])
  list(data = data, desc = desc)
}

#' @export
extractFDAFeatures.Task = function(obj, target = character(0L), feat.methods = list()) {

  stopifnot((hasFunctionalFeatures(obj)))

  data = getTaskData(obj, functionals.as = "matrix")
  target = getTaskTargetNames(obj)

  # Extract features from data
  extracted = extractFDAFeatures.data.frame(obj = data, target = target, feat.methods = feat.methods)

  # And change data so it only contains non-functional features
  task = changeData(obj, extracted$data)
  list(task = task, desc = extracted$desc)
}


#' @export
print.extractFDAFeatDesc = function(x, ...) {
  catf("Extraction of features from functional data:")
  catf("Target: %s", collapse(x$target))
  # FIXME: This could be missunderstood
  catf("Functional Features: %i; Extracted features: %i", length(x$fd.cols), length(x$extractFDAFeat))
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
#' @return [data.frame] or [Task] containing the extracted Features
#' @family extractFDAFeatures
#' @export
reextractFDAFeatures = function(obj, desc) {
  UseMethod("reextractFDAFeatures")
}

#' @export
reextractFDAFeatures.data.frame = function(obj, desc) {
  assertClass(desc, classes = "extractFDAFeatDesc")

  # check for new columns
  new.cols = names(which(names(obj) %nin% desc$coln))
  if (length(new.cols))
    stop("New columns (%s) found in data. Unable to extract.", collapse(new.cols))


  # reextract features using reextractDescription and return
  reextract = Map(
    function(xn, x, fd.col) {
      do.call(x$reextract, c(list(data = obj, target = desc$target, col = fd.col), x$args))
    },
    xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.col = desc$fd.cols)

  # cbind resulting columns. Use data.table to ensure proper naming.
  df = as.data.frame(do.call(cbind, lapply(reextract, setDT)))

  # Reappend target and non-functional features
  keep.cols = setdiff(colnames(obj), desc$fd.cols)
  data = cbind(df, obj[keep.cols])
  data
}


#' @export
reextractFDAFeatures.Task = function(obj, desc) {
  # get data and pass to extractor
  df = getTaskData(obj, functionals.as = "matrix")
  extracted = reextractFDAFeatures.data.frame(df, desc)
  # Change data of task and return task
  changeData(obj, extracted)
}
