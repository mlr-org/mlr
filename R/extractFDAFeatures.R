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
#' @family impute
#' @examples
#' df = data.frame(x = matrix(rnorm(24), ncol = 8), y = factor(c("a", "a", "b")))
#' extracted = extractFDAFeatures(df, target = character(0), fd.features = list(x1 = 1:4, x2=5:8),
#'   fd.grids = list(x1 = 1:4, x2 = 1:4))
#' print(extracted$data)
#' reimpute(data.frame(x = NA_real_), imputed$desc)
extractFDAFeatures = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list(), ...) {
  assertList(feat.methods)
  UseMethod("extractFDAFeatures")
}


#' @export
extractFDAFeatures.data.frame = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list(), ...) {

  # FIXME: Some more checks
  assertList(fd.features)
  assertList(fd.grids)
  assert(all(names(fd.features) == names(fd.grids)))
  assertSubset(unlist(fd.features), choices = colnames(obj))

  # Extract features for every functional covariate
  data = BBmisc::dapply(fd.features, function(x) {
    extractSingleFDAFeatures(obj[[fd.features]], feat.methods[[]])
  })

  desc = makeS3Obj("extractFDAFeatDesc",
    target = target,
    fd.features = fd.features,
    fd.grids = fd.grids,
    feat.methods = feat.methods
  )

  list(data = data, desc = desc)
}

#' @export
extractFDAFeatures.Task = function(obj, target = character(0L), feat.methods = list(),
  fd.features = list(), fd.grids = list()) {

  asserList(feat.methods)
  fd.features = getTaskDescription(obj)$fd.features
  fd.grids = getTaskDescription(obj)$fd.grids
  data = getTaskData(obj)
  target = getTaskTargetNames(obj)

  extractFDAFeatures.data.frame(obj = data, target = target, feat.methods = feat.methods,
    fd.features = fd.features, fd.grids = fd.grids)
}

#' @export
extractSingleFDAFeatures = function() {

}
