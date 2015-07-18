#' @title Confusion matrix.
#'
#' @description
#' Calculates confusion matrix for (possibly resampled) prediction.
#' Rows indicate true classes, columns predicted classes.
#'
#' @param learner [\code{character(1)}]\cr
#'   The name of the learner from caret (cf. \url{http://topepo.github.io/caret/modelList.html}).
#' @param len [\code{integer(1)}]\cr
#'   A length (= precision) parameter which is used by caret to generate its tuning grid.
#' @param x [\code{data.frame}]\cr
#'   The data frame with the input parameters.
#' @param y [\code{factor}|\code{numeric}]\cr
#'   The response vector of the classification / regression problem.
#' @param discretize [\code{logical(1)}]\cr
#'   Should the numerical constraints be discretized (\code{discretize = TRUE})
#'   or should they be defined by their lower and upper bounds
#'   (\code{discretize = FALSE}). The default is \code{discretize = TRUE}.
#' @return [\code{list(2)}]. A list containing two list elements:
#'   the first list element is called \code{par.vals} and it is a list, which
#'   contains all constant tuning parameters, whereas the second list element
#'   is a \code{ParamSet}, containing all the configurable tuning parameters.
#' @export
#' @examples
#' # (1) param set of a caret classification learner (with discretized parameters)
#' library(caret)
#' getCaretParamSet("rf", len = 9L, x = iris[, -5], y = iris[, 5], discretize = TRUE)
#' 
#' # (2) param set of a caret regression learner (without discretized parameters):
#' getCaretParamSet("gbm", len = 9L, x = iris[, 1:3], y = iris[, 4], discretize = FALSE)
getCaretParamSet = function(learner, len = 3, x, y, discretize = TRUE){
  # define caret's var_seq function within this environment
  var_seq = caret::var_seq
  caretGrid = caret::getModelInfo(learner)[[learner]]$grid(x = x, y = y, len = len)

  # transfer caret parameters into mlr parameters
  params = setNames(lapply(colnames(caretGrid), function(i) {
    x = sort(unique(caretGrid[,i]))
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
  }), colnames(caretGrid))

  # are the parameters configurable or are the bounds / values unique?
  isConfigurable = vlapply(params, function(x) {
    (!is.null(x$values) && length(x$values) > 1) |
      (!is.null(x$lower) && !is.null(x$upper) && all(x$upper > x$lower))
  })

  # define par.vals (if existing)
  if (sum(!isConfigurable) == 0) {
    par.vals = NULL
  } else {
    par.vals = lapply(caretGrid[!isConfigurable], function(x) {
      if (class(x) == "factor")
        x = as.character(x)
      return(x[1L])
    })
    # convert integerish variables into integer
    par.vals[vlapply(par.vals, testIntegerish)] = 
      lapply(par.vals[vlapply(par.vals, testIntegerish)], as.integer)
  }

  return(list(par.vals = par.vals,
    par.set = do.call(makeParamSet, params[isConfigurable])))
}
