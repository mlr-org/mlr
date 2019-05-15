#' @title Generate data for critical-differences plot.
#'
#' @description Generates data that can be used to plot a
#' critical differences plot. Computes the critical differences according
#' to either the
#' `"Bonferroni-Dunn"` test or the `"Nemenyi"` test.\cr
#' `"Bonferroni-Dunn"` usually yields higher power as it does not
#' compare all algorithms to each other, but all algorithms to a
#' `baseline` instead. \cr
#' Learners are drawn on the y-axis according to their average rank. \cr
#' For `test = "nemenyi"` a bar is drawn, connecting all groups of not
#' significantly different learners.\cr
#' For `test = "bd"` an interval is drawn arround the algorithm selected
#' as a baseline. All learners within this interval are not signifcantly different
#' from the baseline. \cr
#' Calculation:
#' \deqn{CD = q_{\alpha} \sqrt{\left(\frac{k(k+1)}{6N}\right)}}{CD = q_alpha sqrt(k(k+1)/(6N))} \cr
#' Where \eqn{q_\alpha} is based on the studentized range statistic.
#' See references for details.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param p.value (`numeric(1)`)\cr
#'   P-value for the critical difference. Default: 0.05
#' @param baseline (`character(1)`): (`learner.id`) \cr
#'   Select a `learner.id` as baseline for the `test = "bd"`
#'   ("Bonferroni-Dunn") critical differences
#'   diagram. The critical difference interval will then be positioned arround this learner.
#'   Defaults to best performing algorithm. \cr
#'   For `test = "nemenyi"`, no baseline is needed as it performs *all pairwise
#'   comparisons*.
#' @param test (`character(1)`) \cr
#'   Test for which the critical differences are computed. \cr
#'   \dQuote{bd} for the Bonferroni-Dunn Test, which is comparing all
#'   classifiers to a `baseline`, thus performing a comparison
#'   of one classifier to all others. \cr
#'   Algorithms not connected by a single line are statistically different
#'   from the baseline. \cr
#'   \dQuote{nemenyi} for the [PMCMR::posthoc.friedman.nemenyi.test]
#'   which is comparing all classifiers to each other. The null hypothesis that
#'   there is a difference between the classifiers can not be rejected for all
#'   classifiers that have a single grey bar connecting them.
#' @return (`critDifferencesData`). List containing:
#' \item{data}{(data.frame) containing the info for the descriptive
#'                part of the plot}
#' \item{friedman.nemenyi.test}{(list) of class `pairwise.htest` \cr
#'                                contains the calculated
#'                                [PMCMR::posthoc.friedman.nemenyi.test]}
#' \item{cd.info}{(list) containing info on the critical difference
#'                  and its positioning}
#' \item{baseline}{`baseline` chosen for plotting}
#' \item{p.value}{p.value used for the [PMCMR::posthoc.friedman.nemenyi.test]
#'                  and for computation of the critical difference}
#'
#' @family generate_plot_data
#' @family benchmark
#' @md
#' @export
generateCritDifferencesData = function(bmr, measure = NULL, p.value = 0.05,
  baseline = NULL, test = "bd") {

  assertClass(bmr, "BenchmarkResult")
  assertChoice(test, c("nemenyi", "bd"))
  assertNumeric(p.value, lower = 0, upper = 1, len = 1)
  measure = checkBMRMeasure(measure, bmr)

  # Get Rankmatrix, transpose and get mean ranks
  mean.rank = rowMeans(convertBMRToRankMatrix(bmr, measure))
  # Gather Info for plotting the descriptive part.
  df = data.frame(mean.rank,
    learner.id = names(mean.rank),
    rank = rank(mean.rank, ties.method = "average"))
  # Orientation of descriptive lines yend(=y-value of horizontal line)
  right = df$rank > median(df$rank)
  # Better learners are ranked ascending
  df$yend[!right] = rank(df$rank[!right], ties.method = "first") - 0.5
  # Worse learners ranked descending
  df$yend[right] = rank(-df$rank[right], ties.method = "first") - 0.5
  # Better half of learner have lines to left / others right.
  df$xend = ifelse(!right, 0L, max(df$rank) + 1L)
  # Save orientation, can be used for vjust of text later on
  df$right = as.numeric(right)
  df$short.name = getBMRLearnerShortNames(bmr)

  # Get a baseline
  if (is.null(baseline)) {
    baseline = as.character(df$learner.id[which.min(df$rank)])
  } else {
    assertString(baseline)
    assertChoice(baseline, getBMRLearnerIds(bmr))
  }

  # Perform nemenyi test
  nem.test = friedmanPostHocTestBMR(bmr, measure, p.value)
  # Store Info for plotting the cricital differences
  cd.info = list("test" = test,
    "cd" = nem.test$crit.difference[[test]],
    "x" = df$mean.rank[df$learner.id == baseline],
    "y" = 0.1)

  # Create data for connecting bars (only nemenyi test)
  if (test == "nemenyi") {
    sub = sort(df$mean.rank)
    # Compute a matrix of all possible bars
    mat = apply(t(outer(sub, sub, `-`)), c(1, 2),
      FUN = function(x) ifelse(x > 0 && x < cd.info$cd, x, 0))
    # Get start and end point of all possible bars
    xstart = round(apply(mat + sub, 1, min), 3)
    xend = round(apply(mat + sub, 1, max), 3)
    nem.df = data.table(xstart, xend, "diff" = xend - xstart)
    # For each unique endpoint of a bar keep only the longest bar
    nem.df = nem.df[, .SD[which.max(.SD$diff)], by = "xend"]
    # Take only bars with length > 0
    nem.df = nem.df[nem.df$xend - nem.df$xstart > 0, ]
    # Y-value for bars is between 0.1 and 0..35 hardcoded
    # Descriptive lines for learners start at 0.5, 1.5, ...
    nem.df$y = seq(from = 0.1, to = 0.35, length.out = dim(nem.df)[1])
    cd.info$nemenyi.data = as.data.frame(nem.df)
  }

  makeS3Obj("CritDifferencesData",
    "data" = df,
    "cd.info" = cd.info,
    "friedman.nemenyi.test" = nem.test,
    "baseline" = baseline,
    "p.value" = p.value)
}
#' @title Plot critical differences for a selected measure.
#'
#' @description Plots a critical-differences diagram for all classifiers and
#' a selected measure. If a baseline is selected for the Bonferroni-Dunn
#' test, the critical difference interval will be positioned arround the baseline.
#' If not, the best performing algorithm will be chosen as baseline.
#' The positioning of some descriptive elements can be moved by modifying the
#' generated data.
#'
#' @param obj ([critDifferencesData])
#'   Result of \link{generateCritDifferencesData} function.
#' @param baseline (`character(1)`): ([learner.id]) \cr
#'   Overwrites baseline from \link{generateCritDifferencesData}!\cr
#'   Select a ([learner.id` as baseline for the critical difference
#'   diagram, the critical difference will be positioned arround this learner.
#'   Defaults to best performing algorithm.
#' @template arg_prettynames
#' @template ret_gg2
#'
#' @references Janez Demsar, Statistical Comparisons of Classifiers over Multiple Data Sets,
#' JMLR, 2006
#' @family plot
#' @family benchmark
#' @noMd
#' @export
#' @examples
#' # see benchmark
plotCritDifferences = function(obj, baseline = NULL, pretty.names = TRUE) {

  assertClass(obj, "CritDifferencesData")

  # Plot descritptive lines and learner names
  p = ggplot(obj$data)
  # Point at mean rank
  p = p + geom_point(aes_string("mean.rank", 0, colour = "learner.id"), size = 3)
  # Horizontal descriptive bar
  p = p + geom_segment(aes_string("mean.rank", 0, xend = "mean.rank", yend = "yend",
    color = "learner.id"), size = 1)
  # Vertical descriptive bar
  p = p + geom_segment(aes_string("mean.rank", "yend", xend = "xend",
    yend = "yend", color = "learner.id"), size = 1)
  # Plot Learner name
  if (pretty.names) {
    p = p + geom_text(aes_string("xend", "yend", label = "short.name", color = "learner.id",
      hjust = "right"), vjust = -1)
  } else {
    p = p + geom_text(aes_string("xend", "yend", label = "learner.id", color = "learner.id",
      hjust = "right"), vjust = -1)
  }
  p = p + xlab("Average Rank")
  # Change appearance
  p = p + scale_x_continuous(breaks = c(0:max(obj$data$xend)))
  p = p + theme(axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size = 1),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_blank())

  # Write some values into shorter names as they are used numerous times.
  cd.x = obj$cd.info$x
  cd.y = obj$cd.info$y
  cd = obj$cd.info$cd

  # Plot the critical difference bars
  if (obj$cd.info$test == "bd") {
    if (!is.null(baseline)) {
      assertChoice(baseline, as.character(obj$data$learner.id))
      cd.x = obj$data$mean.rank[obj$data$learner.id == baseline]
    }
    # Add horizontal bar arround baseline
    p = p + annotate("segment", x = cd.x + cd, xend = cd.x - cd, y = cd.y, yend = cd.y,
      alpha = 0.5, color = "darkgrey", size = 2)
    # Add intervall limiting bar's
    p = p + annotate("segment", x = cd.x + cd, xend = cd.x + cd, y = cd.y - 0.05,
      yend = cd.y + 0.05, color = "darkgrey", size = 1)
    p = p + annotate("segment", x = cd.x - cd, xend = cd.x - cd, y = cd.y - 0.05,
      yend = cd.y + 0.05, color = "darkgrey", size = 1)
    # Add point at learner
    p = p + annotate("point", x = cd.x, y = cd.y, alpha = 0.5)
    # Add critical difference text
    p = p + annotate("text", label = stri_paste("Critical Difference =", round(cd, 2), sep = " "),
      x = cd.x, y = cd.y + 0.05)
  } else {
    nemenyi.data = obj$cd.info$nemenyi.data
    if (!(nrow(nemenyi.data) == 0L)) {
      # Add connecting bars
      p = p + geom_segment(aes_string("xstart", "y", xend = "xend", yend = "y"),
        data = nemenyi.data, size = 2, color = "dimgrey", alpha = 0.9)
      # Add text (descriptive)
      p = p + annotate("text",
        label = stri_paste("Critical Difference =", round(cd, 2), sep = " "),
        y = max(obj$data$yend) + .1, x = mean(obj$data$mean.rank))
      # Add bar (descriptive)
      p = p + annotate("segment",
        x = mean(obj$data$mean.rank) - 0.5 * cd,
        xend = mean(obj$data$mean.rank) + 0.5 * cd,
        y = max(obj$data$yend) + .2,
        yend = max(obj$data$yend) + .2,
        size = 2L)
    } else {
      message("No connecting bars to plot!")
    }
  }
  return(p)
}
