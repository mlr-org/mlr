<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
#' @title Get tuning parameters from a learner of the caret R-package.
#'
#' @description
#' Constructs a grid of tuning parameters from a learner of the \code{caret}
#' R-package. These values are then converted into a list of non-tunable
#' parameters (\code{par.vals}) and a tunable
#' \code{\link[ParamHelpers]{ParamSet}} (\code{par.set}), which can be used by
#' \code{\link{tuneParams}} for tuning the learner. Numerical parameters will
#' either be specified by their lower and upper bounds or they will be
#' discretized into specific values.
#'
#' @param learner [\code{character(1)}]\cr
#'   The name of the learner from \code{caret}
#'   (cf. \url{http://topepo.github.io/caret/modelList.html}). Note that the
#'   names in \code{caret} often differ from the ones in \code{mlr}.
#' @param length [\code{integer(1)}]\cr
#'   A length / precision parameter which is used by \code{caret} for
#'   generating the grid of tuning parameters. \code{caret} generates either as
#'   many values per tuning parameter / dimension as defined by \code{length}
#'   or only a single value (in case of non-tunable \code{par.vals}).
#' @param x [\code{data.frame}]\cr
#'   A data frame with the input data.
#' @param y [\code{factor} | \code{numeric}]\cr
#'   A response vector.
#' @param discretize [\code{logical(1)}]\cr
#'   Should the numerical parameters be discretized? Alternatively, they will
#'   be defined by their lower and upper bounds. The default is \code{TRUE}.
#' @return [\code{list(2)}]. A list of parameters:
#' \itemize{
#'   \item{\code{par.vals}} contains a list of all constant tuning parameters
#'   \item{\code{par.set}} is a \code{\link[ParamHelpers]{ParamSet}}, containing all the configurable
#'   tuning parameters
#' }
<<<<<<< HEAD
=======
#' @title Get default parameters from a learner of the caret R-package.
=======
#' @title Get tuning parameters from a learner of the caret R-package.
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd
#'
#' @description
#' Constructs a grid of tuning parameters from a learner of the \code{caret}
#' R-package. These values are then converted into a list of non-tunable
#' parameters (\code{par.vals}) and a tunable
#' \code{\link[ParamHelpers]{ParamSet}} (\code{par.set}), which can be used by
#' \code{\link{tuneParams}} for tuning the learner. Numerical parameters will
#' either be specified by their lower and upper bounds or they will be
#' discretized into specific values.
#'
#' @param learner [\code{character(1)}]\cr
#'   The name of the learner from \code{caret}
#'   (cf. \url{http://topepo.github.io/caret/modelList.html}). Note that the
#'   names in \code{caret} often differ from the ones in \code{mlr}.
#' @param length [\code{integer(1)}]\cr
#'   A length / precision parameter which is used by \code{caret} for
#'   generating the grid of tuning parameters. \code{caret} generates either as
#'   many values per tuning parameter / dimension as defined by \code{length}
#'   or only a single value (in case of non-tunable \code{par.vals}).
#' @param x [\code{data.frame}]\cr
#'   A data frame with the input data.
#' @param y [\code{factor} | \code{numeric}]\cr
#'   A response vector.
#' @param discretize [\code{logical(1)}]\cr
<<<<<<< HEAD
#'   Should the numerical constraints be discretized (\code{discretize = TRUE})
#'   or should they be defined by their lower and upper bounds
#'   (\code{discretize = FALSE}). The default is \code{discretize = TRUE}.
#' @return [\code{list(2)}]. A list of parameters:\cr
#'   (1) \code{par.vals} contains a list of all constant tuning parameters,\cr
#'   (2) \code{par.set} is a \code{\link[ParamHelpers]{ParamSet}}, containing all the configurable
#'   tuning parameters.
>>>>>>> df565b0... convert caret default params to mlr
=======
#'   Should the numerical parameters be discretized? Alternatively, they will
#'   be defined by their lower and upper bounds. The default is \code{TRUE}.
#' @return [\code{list(2)}]. A list of parameters:
#' \itemize{
#'   \item{\code{par.vals}} contains a list of all constant tuning parameters
#'   \item{\code{par.set}} is a \code{\link[ParamHelpers]{ParamSet}}, containing all the configurable
#'   tuning parameters
#' }
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd
=======
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
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
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  params = lapply(colnames(caret.grid), function(i) {
=======
  params = setNames(lapply(colnames(caret.grid), function(i) {
>>>>>>> df565b0... convert caret default params to mlr
=======
  params = lapply(colnames(caret.grid), function(i) {
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd
=======
  params = lapply(colnames(caret.grid), function(i) {
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
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
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
  })
  names(params) = colnames(caret.grid)

  # are the parameters configurable or are the values unique?
  is.tunable = vlapply(params, function(x) {
    (!is.null(x$values) && length(x$values) > 1) |
      (!is.null(x$lower) && !is.null(x$upper) && (x$upper > x$lower))
  })

  # define par.vals (if existing)
  if (all(is.tunable)) {
    par.vals = NULL
  } else {
    par.vals = lapply(caret.grid[!is.tunable], function(x) {
      if (is.factor(x))
<<<<<<< HEAD
=======
  }), colnames(caret.grid))
=======
  })
  names(params) = colnames(caret.grid)
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd

  # are the parameters configurable or are the values unique?
  is.tunable = vlapply(params, function(x) {
    (!is.null(x$values) && length(x$values) > 1) |
      (!is.null(x$lower) && !is.null(x$upper) && (x$upper > x$lower))
  })

  # define par.vals (if existing)
  if (all(is.tunable)) {
    par.vals = NULL
  } else {
<<<<<<< HEAD
    par.vals = lapply(caret.grid[!tunable.index], function(x) {
      if (class(x) == "factor")
>>>>>>> df565b0... convert caret default params to mlr
=======
    par.vals = lapply(caret.grid[!is.tunable], function(x) {
      if (is.factor(x))
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd
=======
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
        x = as.character(x)
      return(x[1L])
    })
    # convert integerish variables into integer
    par.vals[vlapply(par.vals, testIntegerish)] = 
      lapply(par.vals[vlapply(par.vals, testIntegerish)], as.integer)
  }

  return(list(par.vals = par.vals,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    par.set = do.call(makeParamSet, params[is.tunable])))
=======
    par.set = do.call(makeParamSet, params[tunable.index])))
>>>>>>> df565b0... convert caret default params to mlr
=======
    par.set = do.call(makeParamSet, params[is.tunable])))
>>>>>>> f09089e... fixing getCaretParamSet according to hangout with Bernd
=======
    par.set = do.call(makeParamSet, params[is.tunable])))
>>>>>>> ecec28c5da2561fb85d34a04919a749598d77073
}
