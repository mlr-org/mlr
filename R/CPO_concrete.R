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


#' @title Drop constant or near-constant Features.
#'
#' @description
#' Drop all columns that are either constant, or close to constant for numerics,
#' and columns that have only one value for factors or ordered columns.
#'
#' @template cpo_description
#'
#' @param rel.tol [\code{numeric(1)}]\cr
#'   Relative tolerance within which to consider a feature constant.
#'   Set to \code{Inf} to disregard relative tolerance.
#'   Default is \code{1e-8}.
#' @param abs.tol [\code{numeric(1)}]\cr
#'   Absolute tolerance within which to consider a feature constant.
#'   Set to \code{Inf} to disregard absolute tolerance.
#'   Default is \code{1e-8}.
#' @param ignore.na [\code{logical(1)}]\cr
#'   Whether to ignore \code{NA} and \code{NaN} values. If this is
#'   \code{TRUE}, values that are \code{NA} or \code{NaN} will not
#'   be counted as different from any other value. If this is
#'   \code{FALSE}, columns with \code{NA} or \code{NaN} in them will
#'   only count as constant if they are entirely made up of \code{NA},
#'   or entirely made up of \code{NaN}.
#'   Default is \code{FALSE}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoDropConstants = makeCPO("dropconst", rel.tol = 1e-8: numeric[~0, ], abs.tol = 1e-8: numeric[~0, ],
  ignore.na = FALSE: logical,
  .datasplit = "target", cpo.trafo = {
    control = sapply(data, function(col) {
      if (ignore.na) {
        col = col[!(is.na(col) | is.nan(col))]
      }
      if (is.numeric(col)) {
        if (all(col == Inf) || all(col == -Inf) || all(is.nan(col)) || all(is.na(col))) {
          return(FALSE)
        }
        if (any(col == Inf) || any(col == -Inf) || any(is.nan(col)) || any(is.na(col))) {
          return(TRUE)
        }
        cmean = mean(col)
        return(!(all(abs(col - cmean) < abs.tol) || all(abs(col - cmean) / cmean < rel.tol)))
      }
      if (all(is.na(col))) {
        return(FALSE)
      }
      if (any(is.na(col))) {
        return(TRUE)
      }
      return(!all(col == col[1]))
    })
    data[control]
  }, cpo.retrafo = {
    data[control]
  })
