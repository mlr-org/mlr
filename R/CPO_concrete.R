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
  control = list(center = coalesce(attr(result, "scaled:center"), FALSE), scale = coalesce(attr(result, "scaled:scale"), FALSE))
  result
}, cpo.retrafo = {
  scale(as.matrix(data), center = control$center, scale = control$scale)
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
cpoDummyEncode = makeCPO("dummyencode", reference.cat = FALSE: logical, .datasplit = "target",  # nolint
  .properties.needed = "numerics", .properties.adding = c("factors", "ordered"),
  cpo.trafo = {
    lvls = lapply(data, levels)

    cpo.retrafo = function(data) {
      datas = lapply(names(data), function(d) {
        if (is.factor(data[[d]])) {
          df = do.call(data.frame, lapply(lvls[[d]], function(l)
            as.integer(data[[d]] == l)))
          names(df) = paste0(d, lvls[[d]])
          if (reference.cat) {
            # check that no unknown factors occurred
            cleand = data[[d]][!is.na(data[[d]])]
            if (!all(cleand %in% lvls[[d]])) {
              stopf("dummyencode retrafo encountered factor level that wasn't seen before. Try using cpoFixFactors.")
            }
            df[-1]
          } else {
            # unseen factors are ok here.
            df
          }
        } else {
          data[d]
        }
      })
      do.call(cbind, datas)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
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
cpoDropConstants = makeCPO("dropconst", rel.tol = 1e-8: numeric[~0, ], abs.tol = 1e-8: numeric[~0, ],  # nolint
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
registerCPO(cpoDropConstants, "data", "cleanup", "Drop constant or near-constant Features.")

#' @title Clean up Factorial Features.
#'
#' @description
#' Prevent common pitfalls when using factorial data, by making factorial data have the
#' same levels in training and prediction, and by dropping factor levels that do not
#' occur in training data.
#'
#' @template cpo_description
#'
#' @param drop.unused.levels
#'   Factor levels of data that have no instances in the data are dropped. If
#'   \dQuote{fix.factors.prediction} is false, this can lead to training data having
#'   different factor levels than prediction data. Default is \code{TRUE}.
#' @param fix.factors.prediction
#'   Factor levels are kept the same in training and prediction. This is
#'   recommended. Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoFixFactors = makeCPO("fixfactors", drop.unused.levels = TRUE: logical, fix.factors.prediction = TRUE: logical,  # nolint
  .datasplit = "target",
  .properties.needed = "missings",
  cpo.trafo = {
    if (drop.unused.levels) {
      data = droplevels(data)
    }
    control = Filter(function(x) !is.null(x), lapply(data, levels))
    data
  }, cpo.retrafo = {
    if (fix.factors.prediction) {
      data = fixFactors(data, control)
    } else if (drop.unused.levels) {
      data = droplevels(data)
    }
    data
  })
registerCPO(cpoFixFactors, "data", "cleanup", "Clean up Factorial Features.")


#' @title Convert Data into Factors indicating Missing Data
#'
#' @description
#' Convert a data.frame into a data.frame with the same column names,
#' but with columns of factors indicating whether data was missing or not.
#'
#' This is most useful in combination with \code{\link{cpoCbind}}.
#'
#' @param force.dummies [\code{logical(1)}]\cr
#'   Whether to create dummy columns even for data that is not missing.
#'   This can be useful if missing data is expected during test in columns
#'   where it did not occur during training.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoMissingIndicators = makeCPO("missingindicators", force.dummies = FALSE: logical,  # nolint
  .datasplit = "target",
  .properties.needed = "factors",
  .properties.adding = c("numerics", "ordered", "missings"),
  cpo.trafo = {
    dummycols = sapply(data, function(x) any(is.na(x)))
    cpo.retrafo = function(data) {
      data = data[force.dummies | dummycols]
      for (d in names(data)) {
        data[[d]] = as.factor(is.na(data[[d]]))
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoMissingIndicators, "tools", "imputation", "Generate factorial columns indicating whether data was NA.")

