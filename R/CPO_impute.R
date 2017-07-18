
#' @title Impute and re-impute data
#'
#' @description
#' Allows imputation of missing feature values through various techniques.
#' Note that you have the possibility to re-impute a data set
#' in the same way as the imputation was performed during training.
#' This especially comes in handy during resampling when one wants to perform the
#' same imputation on the test set as on the training set.
#'
#' The function \code{impute} performs the imputation on a data set and returns,
#' alongside with the imputed data set, an \dQuote{ImputationDesc} object
#' which can contain \dQuote{learned} coefficients and helpful data.
#' It can then be passed together with a new data set to \code{\link{reimpute}}.
#'
#' The imputation techniques can be specified for certain features or for feature classes,
#' see function arguments.
#'
#' You can either provide an arbitrary object, use a built-in imputation method listed
#' under \code{\link{imputations}} or create one yourself using \code{\link{makeImputeMethod}}.
#'
#' \code{cpoImpute} will impute some columns. \code{cpoImputeAll} behaves just like \code{cpoImpute},
#' except that it will throw an error if there are any missings remaining in its output. \code{cpoImputeAll}
#' should be used if one wants to prepend an imputer to a learner.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{features [\code{character}]}{Feature names (column names of \code{data}).},
#'   \item{classes [\code{character}]}{Feature classes (storage type of \code{data}).}
#'   \item{lvls [\code{named list}]}{Mapping of column names of factor features to their levels,
#'     including newly created ones during imputation.}
#'   \item{impute [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{dummies [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{impute.new.levels [\code{logical(1)}]}{See argument.}
#'   \item{recode.factor.levels [\code{logical(1)}]}{See argument.}
#' }
#'
#' @template cpo_description
#'
#' @param target.cols [\code{character}]\cr
#'   Name of the column(s) specifying the response.
#'   Default is \code{character(0)}.
#' @param classes [\code{named list}]\cr
#'   Named list containing imputation techniques for classes of columns.
#'   E.g. \code{list(numeric = imputeMedian())}.
#' @param cols [\code{named list}]\cr
#'   Named list containing names of imputation methods to impute missing values
#'   in the data column referenced by the list element's name. Overrules imputation set via
#'   \code{classes}.
#' @param dummy.classes [\code{character}]\cr
#'   Classes of columns to create dummy columns for.
#'   Default is \code{character(0)}.
#' @param dummy.cols [\code{character}]\cr
#'   Column names to create dummy columns (containing binary missing indicator) for.
#'   Default is \code{character(0)}.
#' @param dummy.type [\code{character(1)}]\cr
#'   How dummy columns are encoded. Either as 0/1 with type \dQuote{numeric}
#'   or as \dQuote{factor}.
#'   Default is \dQuote{factor}.
#' @param force.dummies [\code{logical(1)}]\cr
#'   Force dummy creation even if the respective data column does not
#'   contain any NAs. Note that (a) most learners will complain about
#'   constant columns created this way but (b) your feature set might
#'   be stochastic if you turn this off.
#'   Default is \code{FALSE}.
#' @param impute.new.levels [\code{logical(1)}]\cr
#'   If new, unencountered factor level occur during reimputation,
#'   should these be handled as NAs and then be imputed the same way?
#'   Default is \code{TRUE}.
#' @param recode.factor.levels [\code{logical(1)}]\cr
#'   Recode factor levels after reimputation, so they match the respective element of
#'   \code{lvls} (in the description object) and therefore match the levels of the
#'   feature factor in the training data after imputation?.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @export
#' @family impute
#' @family CPO
cpoImpute = makeCPO("impute", # nolint
  .par.set = c(
      makeParamSet(makeUntypedLearnerParam("target.cols", default = character(0)),
        makeUntypedLearnerParam("classes", default = list()),
        makeUntypedLearnerParam("cols", default = list()),
        makeUntypedLearnerParam("dummy.classes", default = character(0)),
        makeUntypedLearnerParam("dummy.cols", default = character(0))),
      paramSetSugar(dummy.type = "factor": discrete[numeric, factor],
        force.dummies = FALSE: logical,
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
  .datasplit = "target", # no properties.adding 'missings', since we don't impute all cols
  .properties.needed = "factors",
  cpo.trafo = function(data, target, target.cols, ...) {
    impresult = impute(data, target.cols, ...)
    control = impresult[[2]]
    impresult[[1]]
  }, cpo.retrafo = function(data, control, ...) {
    reimpute(data, control)
  })
registerCPO(cpoImpute, "imputation", "general", "General imputation CPO that uses mlr::impute.")


#' @rdname cpoImpute
#' @export
cpoImputeAll = makeCPO("impute", # nolint
  .par.set = c(
      makeParamSet(makeUntypedLearnerParam("target.cols", default = character(0)),
        makeUntypedLearnerParam("classes", default = list()),
        makeUntypedLearnerParam("cols", default = list()),
        makeUntypedLearnerParam("dummy.classes", default = character(0)),
        makeUntypedLearnerParam("dummy.cols", default = character(0))),
      paramSetSugar(dummy.type = "factor": discrete[numeric, factor],
        force.dummies = FALSE: logical,
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
  .datasplit = "target", .properties.adding = "missings",
  .properties.needed = "factors",
  cpo.trafo = function(data, target, target.cols, ...) {
    impresult = impute(data, target.cols, ...)
    control = impresult[[2]]
    impresult[[1]]
  }, cpo.retrafo = function(data, control, ...) {
    reimpute(data, control)
  })
registerCPO(cpoImputeAll, "imputation", "general", "General imputation CPO that uses mlr::impute and checks that all columns were imputed.")

# 'types':
#   types == NULL: all types
#   otherwise: subset of "numerics", "factors", "ordered"
declareImputeFunction = function(name, method, additional.params, types = NULL) {
  makeCPO(paste0("impute.", name),
    .par.set = c(additional.params,
      paramSetSugar(make.dummy.cols = TRUE: logical,
        force.dummies = FALSE: logical,
        impute.new.levels = TRUE: logical,
        recode.factor.levels = TRUE: logical)),
    .datasplit = "target",
    .properties = c("missings", if (is.null(types)) c("numerics", "factors", "ordered") else types),
    .properties.needed = "factors",
    .properties.adding = "missings",
    cpo.trafo = function(data, target, make.dummy.cols, force.dummies, impute.new.levels, recode.factor.levels, ...) {
      if (ncol(data) == 0) {
        control = "NOCOL"
        return(data)
      }
      imputer = method(...)
      impresult = impute(data, cols = lapply(data, function(dummy) imputer),
        dummy.cols = if (make.dummy.cols) names(data) else character(0),
        force.dummies = force.dummies, impute.new.levels = impute.new.levels,
        recode.factor.levels = recode.factor.levels, dummy.type = "factor")
      control = impresult[[2]]
      impresult[[1]]
    }, cpo.retrafo = function(data, control, ...) {
      if (identical(control, "NOCOL")) {
        return(data)
      }
      reimpute(data, control)
    })
}


#' @title Impute and re-impute data
#'
#' @description
#' Allows imputation of missing feature values through various techniques.
#' Note that you have the possibility to re-impute a data set
#' in the same way as the imputation was performed during training.
#' This especially comes in handy during resampling when one wants to perform the
#' same imputation on the test set as on the training set.
#'
#' @details
#' The description object contains these slots
#' \describe{
#'   \item{target [\code{character}]}{See argument.}
#'   \item{features [\code{character}]}{Feature names (column names of \code{data}).},
#'   \item{classes [\code{character}]}{Feature classes (storage type of \code{data}).}
#'   \item{lvls [\code{named list}]}{Mapping of column names of factor features to their levels,
#'     including newly created ones during imputation.}
#'   \item{impute [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{dummies [\code{named list}]}{Mapping of column names to imputation functions.}
#'   \item{impute.new.levels [\code{logical(1)}]}{See argument.}
#'   \item{recode.factor.levels [\code{logical(1)}]}{See argument.}
#' }
#'
#' @template cpo_description
#'
#' @param make.dummy.cols [\code{logical(1)}]\cr
#'   Whether to create the dummy columns.
#' @param force.dummies [\code{logical(1)}]\cr
#'   Force dummy creation even if the respective data column does not
#'   contain any NAs. Note that (a) most learners will complain about
#'   constant columns created this way but (b) your feature set might
#'   be stochastic if you turn this off.
#'   Default is \code{FALSE}.
#' @param impute.new.levels [\code{logical(1)}]\cr
#'   If new, unencountered factor level occur during reimputation,
#'   should these be handled as NAs and then be imputed the same way?
#'   Default is \code{TRUE}.
#' @param recode.factor.levels [\code{logical(1)}]\cr
#'   Recode factor levels after reimputation, so they match the respective element of
#'   \code{lvls} (in the description object) and therefore match the levels of the
#'   feature factor in the training data after imputation?.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @rdname CPOImputer
#' @name CPOImputer
#' @family impute
#' @family CPO
NULL

#' @export
#' @param const [any]\cr
#'  Constant valued use for imputation.
#' @rdname CPOImputer
cpoImputeConstant = declareImputeFunction("constant", imputeConstant, makeParamSet(makeUntypedLearnerParam("const")))  # nolint
registerCPO(cpoImputeConstant, "imputation", "specialised", "Imputation using a constant value.")

#' @export
#' @rdname CPOImputer
cpoImputeMedian = declareImputeFunction("median", imputeMedian, makeParamSet(), "numerics")  # nolint
registerCPO(cpoImputeMedian, "imputation", "specialised", "Imputation using the median.")

#' @export
#' @rdname CPOImputer
cpoImputeMean = declareImputeFunction("mean", imputeMean, makeParamSet(), "numerics")  # nolint
registerCPO(cpoImputeMean, "imputation", "specialised", "Imputation using the mean.")

#' @export
#' @rdname CPOImputer
cpoImputeMode = declareImputeFunction("mode", imputeMode, makeParamSet())  # nolint
registerCPO(cpoImputeMode, "imputation", "specialised", "Imputation using the mode.")

#' @export
#' @param multiplier [\code{numeric(1)}]\cr
#'   Value that stored minimum or maximum is multiplied with when imputation is done.
#' @rdname CPOImputer
cpoImputeMin = declareImputeFunction("min", imputeMin, paramSetSugar(multiplier = 1: numeric[, ]), "numerics")  # nolint
registerCPO(cpoImputeMin, "imputation", "specialised", "Imputation using constant values shifted below the minimum.")

#' @export
#' @rdname CPOImputer
cpoImputeMax = declareImputeFunction("max", imputeMax, paramSetSugar(multiplier = 1: numeric[, ]), "numerics")  # nolint
registerCPO(cpoImputeMax, "imputation", "specialised", "Imputation using constant values shifted above the maximum.")

#' @export
#' @param min [\code{numeric(1)}]\cr
#'   Lower bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @param max [\code{numeric(1)}]\cr
#'   Upper bound for uniform distribution.
#'   If NA (default), it will be estimated from the data.
#' @rdname CPOImputer
cpoImputeUniform = declareImputeFunction("uniform", imputeUniform, {  # nolint
  ps = paramSetSugar(min = 0: numeric[, ] [[special.vals = list(NA_real_)]],
    max = 0: numeric[, ] [[special.vals = list(NA_real_)]])
  ps$pars$min$default = NA_real_
  ps$pars$max$default = NA_real_
  ps
}, "numerics")
registerCPO(cpoImputeUniform, "imputation", "specialised", "Imputation using uniformly distributed random values.")

#' @export
#' @param mu [\code{numeric(1)}]\cr
#'   Mean of normal distribution. If missing it will be estimated from the data.
#' @param sd [\code{numeric(1)}]\cr
#'   Standard deviation of normal distribution. If missing it will be estimated from the data.
#' @rdname CPOImputer
cpoImputeNormal = declareImputeFunction("normal", imputeNormal, {  # nolint
  ps = paramSetSugar(mu = 0: numeric[, ] [[special.vals = list(NA_real_)]],
    sd = 0: numeric[, ] [[special.vals = list(NA_real_)]])
  ps$pars$mu$default = NA_real_
  ps$pars$sd$default = NA_real_
  ps
}, "numerics")
registerCPO(cpoImputeNormal, "imputation", "specialised", "Imputation using normally distributed random values.")

#' @export
#' @param breaks [\code{numeric(1)} | \dQuote{Sturges}]\cr
#'  Number of breaks to use in \code{\link[graphics]{hist}}.
#'  Defaults to auto-detection via \dQuote{Sturges}.
#' @param use.mids [\code{logical(1)}]\cr
#'  If \code{x} is numeric and a histogram is used, impute with bin mids (default)
#'  or instead draw uniformly distributed samples within bin range.
#' @rdname CPOImputer
cpoImputeHist = declareImputeFunction("hist", imputeHist, paramSetSugar(breaks = "Sturges": integer[1, ] [[special.vals = list("Sturges")]],  # nolint
  use.mids = TRUE: logical))
registerCPO(cpoImputeHist, "imputation", "specialised", "Imputation using random values with probabilities approximating the data.")

#' @export
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'  Supervised learner. Its predictions will be used for imputations.
#'  If you pass a string the learner will be created via \code{\link{makeLearner}}.
#'  Note that the target column is not available for this operation.
#' @param features [\code{character}]\cr
#'  Features to use in \code{learner} for prediction.
#'  Default is \code{NULL} which uses all available features except the target column
#'  of the original task.
#' @rdname CPOImputer
cpoImputeLearner = declareImputeFunction("learner", imputeLearner, makeParamSet(makeUntypedLearnerParam("learner"),  # nolint
  makeUntypedLearnerParam("features", default = NULL)))
registerCPO(cpoImputeLearner, "imputation", "specialised", "Imputation using the response of a classification or regression learner.")
