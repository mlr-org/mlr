#' @param extract [\code{function}]\cr
#'   Function used to extract information from a fitted model during resampling.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   If no function is given, \code{mlr} will automatically apply the following functions for each wrapped learner used: 
#'   \itemize{
#'     \item \code{TuneWrapper}: \code{\link{getTuneResult}}
#'     \item \code{FeatSelWrapper}: \code{\link{getFeatSelResult}}
#'     \item \code{FilterWrapper}: \code{\link{getFilteredFeatures}}
#'   }
