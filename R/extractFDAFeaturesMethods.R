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
makeExtractFDAFeatMethod = function(learn, impute, args = list()) {
  assertFunction(learn, args = c("data", "target", "cols"))
  assertFunction(impute, args = c("data", "target", "cols"))
  assertList(args, names = "named")
  setClasses(list(learn = learn, impute = impute, args = args), "extractFDAFeatMethod")
}
