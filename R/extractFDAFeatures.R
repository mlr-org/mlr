#' @title Extract features from functional data.
#'
#' @description
#' Extract non-functional features from functional features using various methods.
#'
#' The function \code{extractFDAFeatures} performs the extraction for a
#' single functional covariate. Additionally, a \dQuote{extractFDAFeatDesc} object
#' which can contain \dQuote{learned} coefficients and other helpful data for
#' extraction during the predict-phase is returned. This can be used with
#' \code{\link{reExtractFDAFeatures}} to extract features during the prediction phase.
#'
#' You can either provide an arbitrary object, use a built-in method listed
#' under \code{\link{extractFDAFeatures}} or create one yourself using
#' \code{\link{makeExtractFDAFeatMethod}}.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{cols [\code{character}]}{colum names of data}
#'   \item{fd.features [\code{character}]}{Feature names (column names of
#'   \code{data}) for each functional feature.}
#'   \item{fd.grids [\code{character}]}{Measure points (e.g time point for each
#'   column of each functional feature of \code{data})}
#' }
#'
#' @template arg_taskdf
#' @param target [\code{character}]\cr
#'   Name of the column(s) specifying the response.
#'   Default is \code{character(0)}.
#' @param feat.methods [\code{named list}]\cr
#'   List of functional features along with the desired \code{\link{extractFDAFeatures}} methods
#'   for each functional covariate.
#' @return [\code{list}]
#'   \item{data [\code{data.frame}]}{Extracted features.}
#'   \item{desc [\code{extracFDAFeatDesc}]}{Description object.}
#' @export
#' @family
#' @examples
#' df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
#' extracted = extractFDAFeatures(df, target = character(0), fd.features = list(x1 = 1:4, x2=5:8),
#'   fd.grids = list(x1 = 1:4, x2 = 1:4))
#' print(extracted$data)
#' reExtractFDAFeatures(data.frame(x = NA_real_), imputed$desc)
extractFDAFeatures = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list()) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
#' # feat.methods are the function signature that one want to use for the corresponding covariate
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list()) {

  assertSubset(names(feat.methods), names(fd.features))
  assertList(fd.features)
  assertList(fd.grids)
  assert(all(names(fd.features) == names(fd.grids)))
  assertSubset(unlist(fd.features), choices = colnames(obj))

  desc = BBmisc::makeS3Obj("extractFDAFeatDesc",
    target = target,
    coln = colnames(obj),
    colclasses = vcapply(obj, function(x) class(x)[1L]),
    fd.features = fd.features,
    fd.grids = fd.grids,
    extractFDAFeat = namedList(names(feat.methods))
  )

  # Add methods to description
  desc$extractFDAFeat[names(feat.methods)] = feat.methods

  # cleanup the empty list
  desc$extractFDAFeat = Filter(Negate(is.null), desc$extractFDAFeat)

  # Subset fd.features accordingly
  desc$fd.features = desc$fd.features[names(desc$extractFDAFeat)]

  # Assert that all functional features to be transformed are numeric
  assert(unique(vcapply(obj[, unlist(desc$fd.features)], function(x) class(x)[1L])) == "numeric")

  # Apply function from x to all functional features and return as list of
  # lists for each functional feature.
  extractFDAFeat = Map(function(xn, x, fd.cols) {
    list(
      # feats are the extracted features
      feats = do.call(x$learn, c(x$args, list(data = obj, target = target, cols = fd.cols))),
      args = x$args, # Args passed to x$reextract
      reextract = x$reextract  # pass on reextraction learner for extraction in predict phase
    )
  }, xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.cols = desc$fd.features)

  # Append Info relevant for reextraction to desc
  desc$extractFDAFeat = lapply(extractFDAFeat, function(x) {c(x["args"], x["reextract"])})

  # Extract feats for every functional feature and cbind to data.frame
  vals = extractSubList(extractFDAFeat, "feats", simplify = FALSE)
  df = data.frame(do.call(cbind, vals))

  # Reappend target and non-functional features
  data = cbind(df, obj[setdiff(desc$coln, unlist(fd.features[names(feat.methods)]))])
  list(data = data, desc = desc)
}

#' @export
extractFDAFeatures.Task = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list()) {

  assertList(feat.methods)
  # Get data from task / taskdesc
  fd.features = getTaskDesc(obj)$fd.features
  fd.grids = getTaskDesc(obj)$fd.grids
  data = getTaskData(obj)
  target = getTaskTargetNames(obj)

  # Extract features from data
  extracted = extractFDAFeatures.data.frame(obj = data, target = target,
    feat.methods = feat.methods, fd.features = fd.features, fd.grids = fd.grids)

  # Change Task type and other interals to reflect a normal task
  obj = changeFDATaskToNormalTask(obj)
  # And change data so it only contains non-functional features
  task = changeData(obj, extracted$data)
  list(task = task, desc = extracted$desc)
}


#' @export
print.extractFDAFeatDesc = function(x, ...) {
  catf("Extraction of features from functional data:")
  catf("Target: %s", collapse(x$target))
  catf("Functional Features: %i; Extracted features: %i", length(x$fd.features),
    length(x$extractFDAFeat))
}


#' Re-ExtractFDAFeatures a data set
#'
#' This function accepts a data frame or a task and an extractFDAFeatDesc
#' (a FDA feature extraction description)
#' as returned by \code{\link{extractFDAFeatures}} to extract features
#' from previously unseen data.
#'
#' @template arg_taskdf
#' @param desc [\code{extractFDAFeatDesc}]\cr
#'   FDAFeature extraction description as returned by \code{\link{extractFDAFeatures}}
#' @return \code{data.frame} or \code{task} containing the extracted Features
#' @family extractFDAFeatures
#' @export
reExtractFDAFeatures = function(obj, desc) {
  UseMethod("reExtractFDAFeatures")
}

#' @export
reExtractFDAFeatures.data.frame = function(obj, desc) {
  assertClass(desc, classes = "extractFDAFeatDesc")

  # check for new columns
  new.cols = names(which(names(obj) %nin% desc$coln))
  if (length(new.cols))
    stop("New columns (%s) found in data. Unable to extract.", collapse(new.cols))

  # check for same storage type
  classes = vcapply(obj, function(x) class(x)[1L])
  i = intersect(names(classes), names(desc$classes))
  i = which.first(classes[i] != desc$classes[i])
  if (length(i) > 0L) {
    stopf("Some column types have changed, e.g. for column '%s' (expected '%s', got '%s')",
      names(classes[i]), desc$classes[i], classes[i])
  }

  # reExtract features using reExtractDescription and return
  reextract = Map(
    function(xn, x, fd.cols) {
      do.call(x$reextract, c(list(data = obj, target = desc$target, cols = fd.cols), x$args))
      }, xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.cols = desc$fd.features)

  df = do.call(cbind, reextract)
  df = cbind(df, obj[setdiff(desc$coln, unlist(desc$fd.features[names(desc$extractFDAFeat)]))])
  list(data = df, desc = desc)
}


#' @export
reExtractFDAFeatures.Task = function(obj, desc) {
  # get data and pass to extractor
  df = getTaskData(obj)
  extracted = reExtractFDAFeatures.data.frame(df, desc)
  extracted

  # Change Task type and other interals to reflect a normal task
  obj = changeFDATaskToNormalTask(obj)
  # And change data so it only contains non-functional features
  task = changeData(obj, extracted$data)
  list(task = task, desc = extracted$desc)
}
