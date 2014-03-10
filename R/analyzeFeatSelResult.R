#' @title Show and visualized the steps of the feature selection.
#'
#' @description
#' This function prints the steps \code{\link{selectFeatures}} took to find it's optimal set
#' of features and the reason why it stopped.
#' It can also give information about all calculations done on each intermediate step.
#' Currently only implemented for sequential feature selection.
#'
#' @param res [\code{FeatSelResult(1)}]\cr
#'   The result of of \code{\link{selectFeatures}}.
#' @param reduce [\code{logical(1)}]\cr
#'   Per iteration: Print only the selected feature or all features
#'   that were evaluated?
#'   Default is \code{TRUE}.
#' @return Nothing.
#' @export
analyzeFeatSelResult = function(res, reduce=TRUE, printed.features=10L) {
  checkArg(res$control, "FeatSelControlSequential")

  x = res$x
  y = res$y
  op = res$opt.path
  measure = res$opt.path$y.names[1L]
  minimize = res$opt.path$minimize[measure]
  features = names(op$par.set$pars)
  ctrl = res$control
  width.feat = 20L
  width.num = 8L

  ##### print header
  catf("Features         : %i", length(x))
  catf("Performance      : %s", perfsToString(y))
  catf(collapse(x, sep=", "))
  catf("\nPath to optimum:")

  ##### print path

  ### produce df data.frame that contains all info
  df = as.data.frame(op)
  # feature was selected when it never "died" or it "died" in later iteration
  df$sel = (is.na(df$eol) | (df$dob < df$eol))
  df$opt = is.na(df$eol)
  # number of features in set are sum of bits which are 1
  df$n.feats = rowSums(df[,features])
  if(reduce)
    df = df[df$sel, ]

  ### Initialize some variables
  old.feats = features[df[1L, features] == 1]
  old.perf = df[1L, measure]

  ### Iterate over all dobs / steps per dob and print info for each
  for (thedob in unique(df$dob)) {
    df.dob = subset(df, df$dob == thedob)
    df.sel = subset(df.dob, df.dob$sel == TRUE)
    if (!reduce)
    catf(strrepeat("-", 80))
    for (j in seq_row(df.dob)) {
      row = df.dob[j, ]
      cur.perf = row[[measure]]
      cur.diff = cur.perf - old.perf
      cur.feats = features[row[features] == 1]
      cur.sel = ifelse(row$sel, "*", " ")
      change.txt = if (thedob == 1L)
        "Init  "
      else if (length(cur.feats) < length(old.feats))
        "Remove"
      else
        "Add   "
      if (thedob == 1L)
        change.feat = ""
      else
        change.feat = symdiff(cur.feats, old.feats)
      catf("- Features: %4i  %s : %-20s  Perf = %s  Diff: %s  %s",
        length(cur.feats), change.txt, clipString(change.feat, width.feat),
        formatC(cur.perf, format="g", width=width.num),
        formatC(cur.diff, format="g", width=width.num),
        cur.sel)
    }
    # in last block be might not have selected any state because no improvement
    if (nrow(df.sel) > 0L) {
      old.feats = features[df.sel[,features] == 1L]
      old.perf = df.sel[, measure]
    }
  }

  if (!is.na(ctrl$max.features) & (length(x) == ctrl$max.features)) {
    catf("\nStopped, because we reached maximal number of allowed features (%i).", ctrl$max.features)
  } else {
    catf("\nStopped, because no improving feature was found.")
  }
}


