#' @title Show and visualize the steps of feature selection.
#'
#' @description
#' This function prints the steps [selectFeatures] took to find its optimal set
#' of features and the reason why it stopped.
#' It can also print information about all calculations done in each intermediate step.
#'
#' Currently only implemented for sequential feature selection.
#'
#' @template arg_fsres
#' @param reduce (`logical(1)`)\cr
#'   Per iteration: Print only the selected feature (or all features that were evaluated)?
#'   Default is `TRUE`.
#' @template ret_inv_null
#' @family featsel
#' @export
analyzeFeatSelResult = function(res, reduce = TRUE) {

  assertClass(res$control, "FeatSelControlSequential")
  assertFlag(reduce)

  x = res$x
  y = res$y
  op = res$opt.path
  measure = res$opt.path$y.names[1L]
  minimize = res$opt.path$minimize[measure]
  features = names(op$par.set$pars)
  ctrl = res$control
  width.feat = 20L

  ##### print header
  catf("Features         : %i", length(x))
  catf("Performance      : %s", perfsToString(y))
  catf(collapse(x, sep = ", "))
  catf("\nPath to optimum:")

  numToString = function(x) sprintf("%.5g", x)

  ##### print path

  ### produce df data.frame that contains all info
  df = as.data.frame(op)
  # convert measure to text

  # feature was selected when it never "died" or it "died" in later iteration
  df$sel = (is.na(df$eol) | (df$dob < df$eol))
  df$opt = is.na(df$eol)
  # number of features in set are sum of bits which are 1
  df$n.feats = rowSums(df[, features, drop = FALSE])
  if (reduce) {
    df = df[df$sel, , drop = FALSE]
  }

  ### Initialize some variables
  old.feats = features[df[1L, features, drop = TRUE] == 1]
  old.perf = NA_real_

  ### Iterate over all dobs / steps per dob and print info for each
  for (thedob in unique(df$dob)) {
    df.dob = subset(df, df$dob == thedob)
    df.sel = subset(df.dob, df.dob$sel == TRUE)
    if (!reduce) {
      catf(strrepeat("-", 80))
    }
    for (j in seq_row(df.dob)) {
      row = df.dob[j, ]
      cur.feats = features[row[features] == 1]
      cur.sel = ifelse(row$sel, "*", " ")
      cur.perf = row[, measure]
      change.txt = if (thedob == 1L) {
        "Init  "
      } else if (length(cur.feats) < length(old.feats)) {
        "Remove"
      } else {
        "Add   "
      }
      if (thedob == 1L) {
        change.feat = ""
      } else {
        change.feat = symdiff(cur.feats, old.feats)
      }
      catf("- Features: %4i  %s : %-20s  Perf = %s  Diff: %s  %s",
        length(cur.feats), change.txt, clipString(change.feat, width.feat),
        numToString(cur.perf),
        numToString(ifelse(minimize, 1, -1) * (old.perf - cur.perf)),
        cur.sel)
    }
    # in last block be might not have selected any state because no improvement
    if (nrow(df.sel) > 0L) {
      old.feats = features[df.sel[, features, drop = TRUE] == 1L]
      old.perf = df.sel[, measure]
    }
  }

  if (!is.na(ctrl$max.features) & (length(x) == ctrl$max.features)) {
    catf("\nStopped, because we reached maximal number of allowed features (%i).", ctrl$max.features)
  } else {
    catf("\nStopped, because no improving feature was found.")
  }
  invisible(NULL)
}
