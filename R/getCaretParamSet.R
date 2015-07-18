#' @title Get default parameters from a learner of the caret R-package.
#'
#' @description
#' Constructs default parameters from a learner of the \code{caret} R-package
#' and converts them into \code{mlr}.
#'
#' @param learner [\code{character(1)}]\cr
#'   The name of the learner from caret (cf. \url{http://topepo.github.io/caret/modelList.html}).
#' @param length [\code{integer(1)}]\cr
#'   A length (= precision) parameter which is used by \code{caret} when
#'   generating the grid of tuning parameters.
#' @param x [\code{data.frame}]\cr
#'   A data frame with the input parameters.
#' @param y [\code{factor} | \code{numeric}]\cr
#'   A response vector.
#' @param discretize [\code{logical(1)}]\cr
#'   Should the numerical constraints be discretized (\code{discretize = TRUE})
#'   or should they be defined by their lower and upper bounds
#'   (\code{discretize = FALSE}). The default is \code{discretize = TRUE}.
#' @return [\code{list(2)}]. A list of parameters:\cr
#'   (1) \code{par.vals} contains a list of all constant tuning parameters,\cr
#'   (2) \code{par.set} is a \code{\link[ParamHelpers]{ParamSet}}, containing all the configurable
#'   tuning parameters.
#' @export
#' @examples
#' library(caret)
#' 
#' # (1) classification (random forest) with discretized parameters
#' getCaretParamSet("rf", length = 9L, x = iris[, -5], y = iris[, 5], discretize = TRUE)
#' 
#' # (2) regression (gradient boosting machine) without discretized parameters
#' library(mlbench)
#' data(BostonHousing)
#' getCaretParamSet("gbm", length = 9L, x = BostonHousing[, -14],
#'   y = BostonHousing[, 14], discretize = FALSE)
getCaretParamSet = function(learner, length = 3, x, y, discretize = TRUE){
  # define caret's var_seq function within this environment
  var_seq = caret::var_seq
  caret.grid = caret::getModelInfo(learner)[[learner]]$grid(x = x, y = y, len = length)

  # transfer caret parameters into mlr parameters
  params = setNames(lapply(colnames(caret.grid), function(i) {
    x = sort(unique(caret.grid[,i]))
    cl = class(x)
    if (cl == "factor") {
      if (all(levels(x) %in% c("TRUE", "FALSE"))) {
        x = as.logical(as.character(x))
        cl = "logical"
      } else {
        x = as.character(x)
        cl = "character"
      }
    }
    if (discretize)
      cl = "character"
    switch(cl,
      character = makeDiscreteParam(id = i, values = x),
      logical = makeLogicalParam(id = i),
      numeric = makeNumericParam(id = i, lower = min(x), upper = max(x)),
      integer = makeIntegerParam(id = i, lower = min(x), upper = max(x))
    )
  }), colnames(caret.grid))

  # are the parameters configurable or are the bounds / values unique?
  tunable.index = vlapply(params, function(x) {
    (!is.null(x$values) && length(x$values) > 1) |
      (!is.null(x$lower) && !is.null(x$upper) && all(x$upper > x$lower))
  })

  # define par.vals (if existing)
  if (sum(!tunable.index) == 0) {
    par.vals = NULL
  } else {
    par.vals = lapply(caret.grid[!tunable.index], function(x) {
      if (class(x) == "factor")
        x = as.character(x)
      return(x[1L])
    })
    # convert integerish variables into integer
    par.vals[vlapply(par.vals, testIntegerish)] = 
      lapply(par.vals[vlapply(par.vals, testIntegerish)], as.integer)
  }

  return(list(par.vals = par.vals,
    par.set = do.call(makeParamSet, params[tunable.index])))
}
