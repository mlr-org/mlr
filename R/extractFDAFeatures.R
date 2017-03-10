#' @title Extract features from functional data.
#'
#' @description
#' Extract non-functional features from functional features using various methods.
#'
#' The function \code{extractFDAFeatures} performs the extraction for a
#' single functional covariate. Additionally, a \dQuote{extractFDAFeatDesc} object
#' which can contain \dQuote{learned} coefficients and helpful data.
#' It can then be used together with a new data set in order to reimpute
#'
#' The imputation techniques can be specified for each functional feature.
#'
#' You can either provide an arbitrary object, use a built-in method listed
#' under \code{\link{FDAFeatExtracts}} or create one yourself using
#' \code{\link{makeFDAFeatExtractMethod}}.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{fd.eatures [\code{character}]}{Feature names (column names of \code{data}).}
#' }
#'
#' @template arg_taskdf
#' @param target [\code{character}]\cr
#'   Name of the column(s) specifying the response.
#'   Default is \code{character(0)}.
#' @param classes [\code{named list}]\cr
#'   Named list containing imputation techniques for classes of columns.
#'   E.g. \code{list(numeric = imputeMedian())}.
#' @return [\code{list}]
#'   \item{data [\code{data.frame}]}{Extracted features.}
#'   \item{desc [\code{ImputationDesc}]}{Description object.}
#' @export
#' @family
#' @examples
#' df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
#' extracted = extractFDAFeatures(df, target = character(0), fd.features = list(x1 = 1:4, x2=5:8),
#'   fd.grids = list(x1 = 1:4, x2 = 1:4))
#' print(extracted$data)
#' reExtractFDAFeatures(data.frame(x = NA_real_), imputed$desc)
extractFDAFeatures = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list(), ...) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list(), ...) {

  assertSubset(names(feat.methods), names(fd.features))
  assertList(fd.features)
  assertList(fd.grids)
  assert(all(names(fd.features) == names(fd.grids)))
  assertSubset(unlist(fd.features), choices = colnames(obj))

  desc = makeS3Obj("extractFDAFeatDesc",
    target = target,
    fd.features = fd.features,
    fd.grids = fd.grids,
    extractFDAFeat = namedList(names(feat.methods))
  )

  desc$extractFDAFeat[names(feat.methods)] = feat.methods

  # cleanup
  desc$extractFDAFeat = Filter(Negate(is.null), desc$extractFDAFeat)

  # learn and transform
  desc$extractFDAFeat = Map(function(xn, x, fd.cols) {
    list(
      extractFDAFeat = x$FDAExtract,
      feats = do.call(x$learn, c(x$args, list(data = obj, target = target, cols = fd.cols)))
    )
  }, xn = names(desc$extractFDAFeat), x = desc$extractFDAFeat, fd.cols = desc$fd.features)

  vals = extractSubList(desc$extractFDAFeat, "feats", simplify = FALSE)
  df = data.frame(do.call(cbind, vals))
  list(data = df, desc = desc)
}

#' @export
extractFDAFeatures.Task = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list()) {

  assertList(feat.methods)
  # Get data from task / taskdesc
  fd.features = getTaskDescription(obj)$fd.features
  fd.grids = getTaskDescription(obj)$fd.grids
  data = getTaskData(obj)
  target = getTaskTargetNames(obj)

  extractFDAFeatures.data.frame(obj = data, target = target, feat.methods = feat.methods,
    fd.features = fd.features, fd.grids = fd.grids)
}


#' @export
print.extractFDAFeatDesc = function(x, ...) {
  catf("Extraction of features from functional data:")
  catf("Target: %s", collapse(x$target))
  catf("Functional Features: %i; Extracted features: %i", length(x$fd.features), length(x$extractFDAFea))
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
#' @family impute
#' @export
reExtractFDAFeatures = function(obj, desc) {
  UseMethod("reExtractFDAFeatures")
}

#' @export
reExtractFDAFeatures.data.frame = function(obj, desc) {
  assertClass(desc, classes = "extractFDAFeatDesc")

  # check for new columns
  new.cols = names(which(names(x) %nin% desc$cols))
  if (length(new.cols))
    stop("New columns (%s) found in data. Unable to impute.", collapse(new.cols))

  # check for same storage type
  classes = vcapply(x, function(x) class(x)[1L])
  i = intersect(names(classes), names(desc$classes))
  i = which.first(classes[i] != desc$classes[i])
  if (length(i) > 0L) {
    stopf("Some column types have changed, e.g. for column '%s' (expected '%s', got '%s')", names(classes[i]), desc$classes[i], classes[i])
  }

  # restore dropped columns
  x[setdiff(desc$features, names(x))] = NA

  # calculate dummies as nums or factors (see option)
  dummy.cols = lapply(x[desc$dummies], is.na)
  names(dummy.cols) = sprintf("%s.dummy", desc$dummies)
  not.ok = which.first(names(dummy.cols) %in% names(x))
  if (length(not.ok))
    stopf("Dummy column '%s' already present in data", names(dummy.cols)[not.ok])
  dummy.cols = if (desc$dummy.type == "numeric")
    lapply(dummy.cols, as.numeric)
  else
    lapply(dummy.cols, factor, levels = c("FALSE", "TRUE"))

  # check for new levels and replace with NAs
  if (desc$impute.new.levels) {
    cols = names(desc$lvls)
    newlvls = Map(function(x, expected) setdiff(levels(x), expected),
      x = x[cols], expected = desc$lvls)
    newlvls = Filter(length, newlvls)
    if (length(newlvls))
      x[names(newlvls)] = Map(function(x, nl) droplevels(replace(x, x %in% nl, NA)),
        x = x[names(newlvls)], nl = newlvls)
  }

  # actually do the imputation
  cols = intersect(names(x), names(desc$impute))
  x[cols] = Map(
    function(xn, obj) do.call(obj$impute, c(list(data = x, target = desc$target, col = xn), obj$args)),
    xn = cols, obj = desc$impute[cols])

  # recode factor levels
  if (desc$recode.factor.levels) {
    cols = names(desc$lvls)
    x[cols] = Map(function(x, expected) {
      factor(as.character(x), levels = expected)
    }, x = x[cols], expected = desc$lvls)
  }

  x[names(dummy.cols)] = dummy.cols
  data.frame(x, stringsAsFactors = FALSE)
}


#' @export
reExtractFDAFeatures.Task = function(obj, desc) {
  df = getTaskData(obj)
  extracted = reExtractFDAFeatures.data.frame(df, desc)
  # FIXME: convert Task to Normal Task instead
  x = changeData(obj, data = extracted$data)
  x
}
