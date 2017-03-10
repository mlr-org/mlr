#' Create a custom feature extraction method for functional data.
#'
#' This is a constructor to create your own imputation methods.
#' @param learn [\code{function(data, target, cols, ...)}]\cr
#'   Function to learn and extract information on column \code{cols}
#'   out of data frame \code{data}. Argument \code{target} specifies
#'   the target column of the learning task.
#'   The function has to return a named list of values.
#' @param args [\code{list}]\cr
#'   Named list of arguments to pass to \code{learn} via \code{...}.
#' @family extractFDAFeatures
#' @export
makeExtractFDAFeatMethod = function(learn, FDAExtract, args = list()) {
  assertFunction(learn, args = c("data", "target", "cols"))
  assertFunction(FDAExtract, args = c("data", "target", "cols", "vals"))
  assertList(args, names = "named")
  setClasses(list(learn = learn, FDAExtract = FDAExtract, args = args), "extractFDAFeatMethod")
}


# helper function to extract a single vector of length(nObs) from the data
simpleFDAExtract = function(data, target, cols, vals) {
  if (is.na(vals))
    stopf("Error extracting feature from column '%s' to '%t'. Maybe all input data was missing?",
      which.first(colnames(data) %in% cols, use.names = TRUE),
      which.last(colnames(data) %in% cols, use.names = TRUE))

  x = data[[cols]]

  # cast logicals to factor if required
  if (is.logical(vals)) {
    vals = as.factor(vals)
  }
  vals
}


#' Built-in imputation methods.
#'
#' The built-ins are:
#' \itemize{
#'   \item \code{extractFDAMedian()} for extracting the median from a curve.
#'   \item \code{extractFDAMean()} for extracting the mean from a curve.
#' @name extractFDAFeatures
#' @rdname extractFDAFeatures
#' @family extractFDAFeatures
NULL


#' @export
#' @rdname extractFDAFeatures
extractFDAMedian = function() {
  makeExtractFDAFeatMethod(
    learn = function(data, target, cols) {apply(data[, cols], 1, median, na.rm = TRUE)},
    FDAExtract = function(data, target, cols) {apply(data[, cols], 1, median, na.rm = TRUE)}
  )
}

#' @export
#' @rdname extractFDAFeatures
extractFDAMean = function() {
  makeExtractFDAFeatMethod(
    learn = function(data, target, cols) {apply(data[, cols], 1, mean, na.rm = TRUE)},
    FDAExtract = function(data, target, cols) {apply(data[, cols], 1, mean, na.rm = TRUE)}
  )
}
