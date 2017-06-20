#' @include ParamSetSugar.R

#' @title Construct a CPO for PCA preprocessing
#'
#' @template cpo_description
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center the data before performing PCA.
#'   Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale the data before performing PCA. The centering / scaling algorithm
#'   of R's dQuote{scale} is used.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoPca = makeCPO("pca", center = TRUE: logical, scale = FALSE: logical, .datasplit = "numeric", cpo.trafo = {  # nolint
  pcr = prcomp(as.matrix(data), center = center, scale. = scale)
  data = as.data.frame(pcr$x)
  control = list(rotation = pcr$rotation, center = pcr$center, scale = pcr$scale)
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data), center = control$center, scale = control$scale) %*% control$rotation)
})
registerCPO(cpoPca, "data", "numeric data preprocessing", "Perform Principal Component Analysis (PCA) using stats::prcomp.")

#' @title Construct a CPO for scaling / centering
#'
#' @template cpo_description
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center the data.
#'   Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale the data.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScale = makeCPO("scale", center = TRUE: logical, scale = TRUE: logical, .datasplit = "numeric", cpo.trafo = {  # nolint
  result = scale(as.matrix(data), center = center, scale = scale)
  data[] = result
  control = list(center = coalesce(attr(result, "scaled:center"), FALSE), scale = coalesce(attr(result, "scaled:scale"), FALSE))
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data), center = control$center, scale = control$scale))
})
registerCPO(cpoScale, "data", "numeric data preprocessing", "Center and / or scale the data using base::scale.")

#' @title CPO Dummy Encoder
#'
#' @template cpo_description
#'
#' @param reference.cat [\code{logical}]\cr
#'   If \dQuote{reference.cat} is \code{TRUE}, the first level of every factor column
#'   is taken as the reference category and the encoding is \code{c(0, 0, 0, ...)}.
#'   If this is \code{FALSE}, the encoding is always one-hot-encoding. Default is \code{FALSE}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoDummyEncode = makeCPO("dummyencode", reference.cat = FALSE: logical, .datasplit = "target", .fix.factors = TRUE,  # nolint
  .properties.needed = "numerics", .properties.adding = c("factors", "ordered"),
  cpo.trafo = {
  mf = stats::model.frame(~., data)
  control = list()
  control = attr(mf, "terms")
  if (!reference.cat) {
    attr(control, "intercept") = 0
  }
  prev.action = options()$na.action
  options(na.action = "na.pass")
  result = as.data.frame(model.matrix(control, data))
  options(na.action = prev.action)
  if (reference.cat) {
    result[-1]
  } else {
    result
  }
}, cpo.retrafo = {
  prev.action = options()$na.action
  options(na.action = "na.pass")
  result = as.data.frame(model.matrix(control, data))
  options(na.action = prev.action)
  if (reference.cat) {
    result[-1]
  } else {
    result
  }

})
registerCPO(cpoDummyEncode, "data", "feature conversion", "Convert factorial columns to numeric columns by dummy encoding them")

#' @title Drop All Columns Except Certain Selected Ones from Data
#'
#' @description
#' Select columns by type or name. The parameters \dQuote{type} and
#' \dQuote{pattern} are additive; if both are given, all column that match
#' either will be returned.
#'
#' @template cpo_description
#'
#' @param type [\code{character}]\cr
#'   One or more out of \dQuote{numeric}, \dQuote{ordered}, \dQuote{factor}, \dQuote{other}.
#'   The type of columns to keep. Default is \code{character(0)}.
#' @param index [\code{integer}]\cr
#'   Indices of columns to keep. Note that the index counts columns without the target column(s).
#'   This and the next parameter make it possible to re-order columns. While all columns which match either
#'   \dQuote{type}, \dQuote{pattern} or \dQuote{index} remain in the resulting data, the ones
#'   selected by \dQuote{index} are put at the front in the order specified.
#'   Default is \code{integer(0)}.
#' @param names [\code{character}]\cr
#'   Names of columns to keep. Matching columns will be kept in order of their names occurring, but after
#'   the columns indicated in \dQuote{index}.
#' @param pattern [\code{character(1)}]\cr
#'   A pattern to match against the column names. Same as in \code{\link{grep}}.
#'   Default is \code{NULL} for no matching.
#' @param pattern.ignore.case [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Whether to perform case insensitive matching. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param pattern.perl [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Should Perl-compatible regexps be used? Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param pattern.fixed [\code{logical(1)}]\cr
#'   Influences behaviour of \dQuote{pattern}: Whether to use match \code{pattern} as as is. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param invert [\code{logical(1)}]\cr
#'   Invert column selection: Drop the named columns and return the rest, instead of keeping the selected
#'   columns only. Default is \code{FALSE}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoSelect = makeCPO("select",  # nolint
  .par.set = c(
      paramSetSugar(type = list(): discrete[numeric, ordered, factor, other]^NA,
        index = integer(0): integer[1, ]^NA),
      makeParamSet(makeUntypedLearnerParam("names", default = character(0)),
        makeCharacterParam("pattern", NULL, special.vals = list(NULL))),
      paramSetSugar(
          pattern.ignore.case = FALSE: logical [[requires = quote(!is.null(pattern))]],
          pattern.perl = FALSE: logical [[requires = quote(!is.null(pattern))]],
          pattern.fixed = FALSE: logical [[requires = quote(!is.null(pattern))]],
          invert = FALSE: logical)),
  .datasplit = "target", cpo.trafo = {
    assertCharacter(names, any.missing = FALSE, unique = TRUE)
    assertIntegerish(index, any.missing = FALSE, unique = TRUE)

    index = getColIndices(data, type, index, names, pattern, invert, pattern.ignore.case, pattern.perl, pattern.fixed)

    cpo.retrafo = function(data) {
      data[index]
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoSelect, "data", "feature selection ", "Select features from a data set by type, column name, or column index.")

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
#' @template arg_taskdf
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
#' @return [\code{list}]
#'   \item{data [\code{data.frame}]}{Imputed data.}
#'   \item{desc [\code{ImputationDesc}]}{Description object.}
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
