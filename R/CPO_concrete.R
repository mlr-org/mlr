#' @include ParamSetSugar.R

#' @title Construct a CPO for PCA preprocessing
#'
#' @template cpo_description
#'
#' @description
#' Note that data is neither scaled nor centered. Often
#' this needs to be done for PCA, in which case, \code{\link{cpoScale}}
#' should be used in addition (and before) \code{cpoPca}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoPca = makeCPO("pca", .datasplit = "numeric", cpo.trafo = {  # nolint
  pcr = prcomp(as.matrix(data), center = FALSE, scale. = FALSE)
  data = as.data.frame(pcr$x)
  control = list(rotation = pcr$rotation)
  data
}, cpo.retrafo = {
  as.data.frame(as.matrix(data) %*% control$rotation)
})
registerCPO(cpoPca, "data", "numeric data preprocessing", "Perform Principal Component Analysis (PCA) using stats::prcomp.")

#' @title Apply a Function Element-Wise
#'
#' @description
#' The function must either vectorize over the given data,
#' or will be applied to each data element on its own.
#'
#' It must not change the type of the data, i.e. numeric
#' data must remain numeric etc.
#'
#' If the function can only handle a subset of the given columns,
#' e.g. only a certain type, use \code{affect.*} arguments.
#'
#' @template cpo_description
#'
#' @param fun [\code{function}]\cr
#'   The function to apply. Must take one argument. If
#'   \code{vectorize} is \code{TRUE}, the argument is the
#'   whole column, \code{fun} must vectorize over it;
#'   otherwise, the function gets called once for
#'   every data item, and both the function argument and
#'   the return value must have length 1.
#' @param vectorize [\code{logical(1)}]\cr
#'   Whether to call \code{fun} once for each column, or
#'   once for each element. If \code{fun} vectorizes,
#'   it is recommended to have this set to \code{TRUE}
#'   for better performance. Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoApplyFun = makeCPO("fun.apply",  # nolint
  .par.set = makeParamSet(
      makeFunctionLearnerParam("fun"),
      makeLogicalLearnerParam("vectorize", default = TRUE)),
  .datasplit = "target", cpo.trafo = {
    if (vectorize) {
      fun2 = fun
    } else {
      fun2 = function(col) {
        sapply(col, function(x) {
          ret = fun(x)
          if (length(ret) != 1) {
            stop("cpoApplyFun 'fun' did not return a result with length 1")
          }
        })
      }
    }
    cpo.retrafo = function(data) {
      as.data.frame(lapply(data, fun2))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoPca, "data", "general data preprocessing", "Apply an arbitrary function column-wise.")


#' @title Range Scaling CPO
#'
#' @description
#' Linearly transform data columns so they are
#' between \code{lower} and \code{upper}. If
#' \code{lower} is greater than \code{upper},
#' this will reverse the ordering of input data.
#' \code{NA}, \code{Inf} are ignored.
#'
#' @template cpo_description
#'
#' @param lower [\code{numeric(1)}]\cr
#'   Target value of smallest item of input data.
#'   Default is 0.
#' @param scale [\code{numeric(1)}]\cr
#'   Target value of greatest item of input data.
#'   Default is 1.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScaleRange = makeCPO("range.scale", lower = 0: numeric[~., ~.], upper = 1: numeric[~., ~.],  # nolint
  .datasplit = "numeric",
  cpo.trafo = {
    ranges = lapply(data, function(x) {
      rng = range(x, na.rm = TRUE, finite = TRUE)
      # linear transformation to get minimum to 'lower' and maximum to 'upper:
      # x' = a + x * b
      # where b is (upper - lower) / (max(x) - min(x))
      # and   a is -min(x) * b + lower
      b = (upper - lower) / (rng[2] - rng[1])
      a = -rng[1] * b + lower
      c(a, b)
    })
    cpo.retrafo = function(data) {
      for (i in seq_along(data)) {
        trafo = ranges[[i]]
        data[[i]] = trafo[1] + data[[i]] * trafo[2]
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoScaleRange, "data", "numeric data preprocessing", "Scale numeric columns to lie in a given range.")

#' @title Max Abs Scaling CPO
#'
#' @description
#' Scale the numeric data columns so their maximum absolute value
#' is \code{maxabs}, if possible. \code{NA}, \code{Inf} are ignored, and features that are constant 0
#'   are not scaled.
#'
#' @template cpo_description
#'
#' @param maxabs [\code{numeric(1)}]\cr
#'   The maximum absolute value for each column after transformation. Default is 1.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScaleMaxAbs = makeCPO("maxabs.scale", maxabs = 1: numeric[0, ~.],  # nolint
  .datasplit = "numeric", .retrafo.format = "combined",
  cpo.trafo = {
    scaling = lapply(data, function(x) {
      s = max(abs(range(x, na.rm = TRUE, finite = TRUE)))
      if (s == 0) {
        s = 1
      }
      s
    })
    function(data) {
      for (i in seq_along(data)) {
        data[[i]] = data[[i]] / scaling[[i]] * maxabs
      }
      data
    }
  }, cpo.retrafo = NULL)
registerCPO(cpoScaleMaxAbs, "data", "numeric data preprocessing", "Scale numeric columns to get a specific maximum absolute value.")

#' @title Scale Rows to Unit Length
#'
#' @description
#' Normalizes the data row-wise. This is a natural
#' generalization of the "sign" function to higher
#' dimensions.
#'
#' @template cpo_description
#'
#' @param length [\code{numeric(1)}]\cr
#'   Length to scale rows to. Default is 1.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoSpatialSign = makeCPO("spatial.sign", length = 1: numeric[0, ~.], .datasplit = "numeric", .retrafo.format = "stateless",  # nolint
  .properties = c("numerics", "factors", "ordered"),  # no missings
  cpo.trafo = NULL, cpo.retrafo = {
    t(apply(as.matrix(data), 1, function(x) {
      len = sqrt(sum(x ^ 2))
      if (!identical(len, 0)) {
        x = x / len * length
      }
      x
    }))
  })
registerCPO(cpoSpatialSign, "data", "numeric data preprocessing", "Scale numeric rows to given length.")

#' @title Probability Encoding
#'
#' @description
#' TODO: what is this actually called?
#'
#' Converts factor columns into columns giving the probability
#' for each target class to have this target, given the column value.
#'
#' @template cpo_description
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoProbEncode = makeCPO("prob.encode",  # nolint
  .datasplit = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = c("twoclass", "multiclass", "classif"),
  .fix.factors = TRUE,
  cpo.trafo = {
    probs = lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl)
        sapply(c(levels(col), NA), function(cl)
          mean(target[[1]][is.na(cl) | col == cl] == tl))))

    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
        dummyenc %*% probs[[idx]]
      }))
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoProbEncode, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by probability encoding them")

#' @title Impact Encoding
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @template cpo_description
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoImpactEncodeClassif = makeCPO("impact.encode.classif",  # nolint
  smoothing = 1e-4: numeric[~0, ~.],
  .datasplit = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = c("twoclass", "multiclass", "classif"),
  .fix.factors = TRUE,
  cpo.trafo = {
    probs = lapply(data, function(col)
      sapply(levels(target[[1]]), function(tl) {
        tprop = mean(target[[1]] == tl)
        tplogit = log(tprop / (1 - tprop))
        sapply(c(levels(col), NA), function(cl) {
          condprob = (sum(target[[1]][is.na(cl) | col == cl] == tl) + smoothing) / (sum(is.na(cl) | col == cl) + 2 * smoothing)
          cplogit = log(condprob / (1 - condprob))
          cplogit - tplogit
        })
      }))

    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        dummyenc = sapply(levels(curdat), function(lvl) as.integer(curdat == lvl))
        dummyenc %*% probs[[idx]]
      }))
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoImpactEncodeClassif, "data", "feature conversion", "Convert factorial columns in classification tasks to numeric columns by impact encoding them")


#' @title Impact Encoding
#'
#' @description
#' Impact encoding as done by vtreat
#'
#' @template cpo_description
#'
#' @param smoothing [\code{numeric(1)}]\cr
#'   A finite positive value used for smoothing.
#'   Default is \code{1e-4}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoImpactEncodeRegr = makeCPO("impact.encode.regr",  # nolint
  smoothing = 1e-4: numeric[~0, ~.],
  .datasplit = "factor",
  .properties.adding = c("factors", "ordered"),
  .properties.needed = "numerics",
  .properties.target = "regr",  # TODO: multiclass?
  .fix.factors = TRUE,
  cpo.trafo = {
    meanimp = mean(target[[1]])
    impact = lapply(data, function(col)
      c(sapply(levels(col), function(lvl) {
        (sum(target[[1]][col == lvl]) + smoothing * meanimp) / (sum(col == lvl) + smoothing) - meanimp
      }), .TEMP.MISSING = meanimp))
    cpo.retrafo = function(data) {
      ret = do.call(cbind, lapply(seq_along(data), function(idx) {
        curdat = data[[idx]]
        levels(curdat) = c(levels(curdat), ".TEMP.MISSING")
        curdat[is.na(curdat)] = ".TEMP.MISSING"
        impact[[idx]][curdat]
      }))
      colnames(ret) = colnames(data)
      rownames(ret) = rownames(data)
      as.data.frame(ret)
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoImpactEncodeRegr, "data", "feature conversion", "Convert factorial columns in regression tasks to numeric columns by impact encoding them")

#' @title Compine Rare Factors
#'
#' @template cpo_description
#'
#' @param max.collapsed.class.prevalence [\code{numeric(1)}]\cr
#'   Maximum prevalence of newly created collapsed factor level.
#'   Default is \code{0.1}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoCollapseFact = makeCPO("collapse.fact",  # nolint
  max.collapsed.class.prevalence = 0.1: numeric[0, ~1],
  .datasplit = "factor",
  cpo.trafo = {
    newlevels = sapply(data, function(d) {
      if (all(is.na(d))) {
        return(levels(d))
      }
      fractions = cumsum(sort(table(d))) / sum(!is.na(d))
      collapse = names(fractions)[fractions < max.collapsed.class.prevalence]
      if (length(collapse) > 1) {
        nocollapse = setdiff(levels(d), collapse)
        lvls = list(collapsed = collapse)
        insert(lvls, stats::setNames(as.list(nocollapse), nocollapse))
      } else {
        levels(d)
      }
    }, simplify = FALSE)
    cpo.retrafo = function(data) {
      for (n in names(data)) {
        levels(data[[n]]) = newlevels[[n]]
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoCollapseFact, "data", "factor data preprocessing", "Combine rare factors.")

#' @title Split Numeric Features into Quantile Bins
#'
#' @template cpo_description
#'
#' @param num.bins [\code{numeric(1)}]\cr
#'   Number of bins to create. Default is \code{2}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoQuantileBinNumerics = makeCPO("bin.numerics", numsplits = 2: integer[2, ],  # nolint
  .properties.needed = "ordered", .properties.adding = "numerics",
  .datasplit = "numeric", cpo.trafo = {
    breaks = lapply(data, function(d)
      unique(c(-Inf, quantile(d, (1:(numsplits - 1)) / numsplits, na.rm = TRUE), Inf)))
    cpo.retrafo = function(data) {
      as.data.frame(mapply(function(d, b) ordered(cut(d, breaks = b)), d = data, b = breaks, SIMPLIFY = FALSE),
        row.names = rownames(data))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)
registerCPO(cpoCollapseFact, "data", "feature conversion", "Convert Numerics to Ordered by binning.")

#' @title Convert All Features to Numerics
#'
#' @template cpo_description
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoAsNumeric = makeCPO("as.numeric", .properties.adding = c("factors", "ordered"), .properties.needed = "numerics",  # nolint
  .retrafo.format = "stateless", .datasplit = "factor", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) })
registerCPO(cpoCollapseFact, "data", "feature conversion", "Convert all Features to Numerics using as.numeric.")

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
#' @template cpo_description
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

#' @title Create a "model matrix" from the data given a formula
#'
#' This uses the \dQuote{stats} function \code{model.matrix} to create
#' (numerical) data from the given data, using the provided formula.
#'
#' @template cpo_description
#'
#' @param formula [\code{formula}]\cr
#'   Formula to use. Higher order interactions can be created using constructs
#'   like \code{~. ^ 2}.
#'
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoModelMatrix = makeCPO("model.matrix", .fix.factors = TRUE, .retrafo.format = "stateless",  # nolint
  .par.set = makeParamSet(makeUntypedLearnerParam("formula")), .datasplit = "target",
  .properties.adding = c("factors", "ordered"), .properties.needed = "numerics",
  cpo.trafo = NULL, cpo.retrafo = {
    as.data.frame(model.matrix(formula, data = data))
  })
registerCPO(cpoSelect, "data", "general", ".")


