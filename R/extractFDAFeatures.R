#' @title Extract features from functional data.
#'
#' @description
#' Extract non-functional features from functional features using various methods. \cr
#' The function \code{extractFDAFeatures} performs the extraction for all functional features
#' and respective methods specified in \code{feat.methods}. \cr
#' Additionally, a \dQuote{\code{extractFDAFeatDesc}} object
#' which contains \code{learned} coefficients and other helpful data for
#' extraction during the predict-phase is returned. This can be used with
#' \code{\link{reExtractFDAFeatures}} in order to extract features during the prediction phase. \cr
#' As \code{feat.methods}, either a method created using \code{\link{makeExtractFDAFeatMethod}},
#' a built-in method listed or one of the buit-in methods listed in
#' \code{\link{extractFDAFeatMethods}} can be supplied.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{coln [\code{character}]}{colum names of data.}
#'   \item{fd.cols [\code{character}]}{Functional feature names.}
#'   \item{colclasses [\code{character}]}{Classes of the features.}
#'   \item{extractFDAFeat [\code{list}]}{Contains \code{feature.methods} and relevant
#'   parameters for reExtraction}.
#' }
#'
#' @param target [\code{character}]\cr
#' Task target column. Only neccessary for data.frames, default: \code{character(0)}.
#' @param feat.methods [\code{named list}]\cr
#'   List of functional features along with the desired \code{\link{extractFDAFeatures}} methods
#'   for each functional feature. A signature for the desired function can be provided for
#'   every covariable. Multiple functions for a  single covariable are not allowed.
#'   Specifying \code{feat.methods} = "all" applies the \code{extratFDAFeatures} method to each
#'   functional feature. Names of \code{feat.methods} must match column names of functional features.
#' @return [\code{list}]
#'   \item{data [\code{data.frame}]}{Extracted features.}
#'   \item{desc [\code{extracFDAFeatDesc}]}{Description object. See description for details.}
#' @family extractFDAFeatures
#' @export
#' @examples
#' df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
#' fdf = makeFunctionalData(df, fd.features = list(x1 = 1:4, x2=5:8), exclude.cols = "y")
#' t = makeClassifTask(data = fdf, target = "y")
#' extracted = extractFDAFeatures(t,
#' feat.methods = list("x1" = extractFDAMean(), "x2" = extractFDAMinMax()))
#' print(extracted$task)
#' reExtractFDAFeatures(t, extracted$desc)

extractFDAFeatures = function(obj, target = character(0L), feat.methods = list()) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list()) {

  fdf = getFunctionalFeatures(obj)
  assertDataFrame(fdf, min.cols = 1L)

  # If the same transform should be applied to all features, rep method and name accordingly
  # "all" needs to be first list name.
  if (names(feat.methods)[1] == "all")
    feat.methods = setNames(rep(feat.methods, ncol(fdf)), names(fdf))

  desc = BBmisc::makeS3Obj("extractFDAFeatDesc",
    target = target,
    coln = colnames(obj),
    fd.cols = names(which(vcapply(obj, function(x) class(x)[1L]) == "matrix")),
    colclasses = vcapply(obj, function(x) class(x)[1L]),
    extractFDAFeat = namedList(names(feat.methods))
  )

  desc$extractFDAFeat[names(feat.methods)] = feat.methods
  # cleanup the empty list
  desc$extractFDAFeat = Filter(Negate(is.null), desc$extractFDAFeat)
  # Subset fd.cols accordingly
  desc$fd.cols = names(desc$extractFDAFeat)

  # Apply function from x to all functional features and return as list of
  # lists for each functional feature.
  extracts = Map(function(xn, x, fd.cols) {
    list(
      # feats are the extracted features
      feats = do.call(x$learn, c(x$args, list(data = obj, target = target, cols = fd.cols))),
      args = x$args, # Args passed to x$reextract
      reextract = x$reextract  # pass on reextraction learner for extraction in prediction
    )
  }, xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.cols = desc$fd.cols)

  # Append Info relevant for reextraction to desc
  desc$extractFDAFeat = lapply(extracts, function(x) {c(x["args"], x["reextract"])})

  # Extract feats for every functional feature and cbind to data.frame
  vals = extractSubList(extracts, "feats", simplify = FALSE)

  if (!any(vlapply(vals, is.data.frame))) {
    stop("feat.method needs to return a data.frame with one row
      per observation in the original data.")
  } else if (any(unique(vnapply(vals, nrow)) != nrow(obj))) {
    stop("feat.method needs to return a data.frame with one row
      per observation in the original data and equal nrow per column.")
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
  catf("Functional Features: %i; Extracted features: %i", length(x$fd.cols),
    length(x$extractFDAFeat))
}


#' Re-ExtractFDAFeatures a data set
#'
#' @description
#' This function accepts a data frame or a task and an extractFDAFeatDesc
#' (a FDA feature extraction description)
#' as returned by \code{\link{extractFDAFeatures}} to extract features
#' from previously unseen data.
#'
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


  # reExtract features using reExtractDescription and return
  reextract = Map(
    function(xn, x, fd.cols) {
      do.call(x$reextract, c(list(data = obj, target = desc$target, cols = fd.cols), x$args))
    },
    xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.cols = desc$fd.cols)

  # cbind resulting columns. Use data.table to ensure proper naming.
  df = as.data.frame(do.call(cbind, lapply(reextract, setDT)))

  # Reappend target and non-functional features
  keep.cols = setdiff(colnames(obj), desc$fd.cols)
  data = cbind(df, obj[keep.cols])
  data
}


#' @export
reExtractFDAFeatures.Task = function(obj, desc) {
  # get data and pass to extractor
  df = getTaskData(obj, functionals.as = "matrix")
  extracted = reExtractFDAFeatures.data.frame(df, desc)
  # Change data of task and return task
  changeData(obj, extracted)
}
