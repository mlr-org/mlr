#' @title Generate data for critical-differences plot.
#'
#' @description Generates data that can be used to plot a
#' critical differences plot. Computes the critical differences according
#' to either the
#' \code{"Bonferroni-Dunn"} test or the \code{"Nemenyi"} test.\cr
#' \code{"Bonferroni-Dunn"} usually yields higher power as it does not
#' compare all algorithms to each other, but all algorithms to a
#' \code{baseline} instead. \cr
#' Learners are drawn on the y-axis according to their average rank. \cr
#' For \code{test = "nemenyi"} a bar is drawn, connecting all groups of not
#' significantly different learners.\cr
#' For \code{test = "bd"} an interval is drawn arround the algorithm selected
#' as baseline. All learners within this interval are not signifcantly different
#' from the baseline. \cr
#' Calculation:
#' \deqn{ CD = q_{\alpha} \sqrt{(\frac{k(k+1)}{6N})}}{CD = q_alpha sqrt(k(k+1)/(6N))} \cr
#' Where \eqn{q_\alpha} is based on the  studentized range statistic.
#' See references for details.
#'
#' @template arg_bmr
#' @template arg_measure
#' @param p.value [\code{numeric}(1)]\cr
#'   P-value for the critical difference. Default: 0.05
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#'   Select a \code{learner.id} as baseline for the \code{test = "bd"}
#'   ("Bonferroni-Dunn") critical differences
#'   diagram.The critical difference Interval will then be positioned arround this learner.
#'   Defaults to best performing algorithm. \cr
#'   For \code{test = "nemenyi"}, no baseline is needed as it performs \code{all pairwise
#'   comparisons.}
#' @param test [\code{character(1)}] \cr
#'   Test for which the critical differences are computed. \cr
#'   \dQuote{bd} for the Bonferroni-Dunn Test, which is comparing all
#'   classifiers to a \code{baseline}, thus performing a comparison
#'   of one classifier to all others. \cr
#'   Algorithms not connected by a single line are statistically different.
#'   then the baseline. \cr
#'   \dQuote{nemenyi} for the \code{\link[PMCMR]{posthoc.friedman.nemenyi.test}}
#'   which is comparing all classifiers to each other. The null hypothesis that
#'   there is a difference between the classifiers can not be rejected for all
#'   classifiers that have a single grey bar connecting them.
#' @return [\code{critDifferencesData}]. List containing:
#' \item{data}{[\code{data.frame}] containing the info for the descriptive
#'                part of the plot}
#' \item{friedman.nemenyi.test}{[\code{list}] of class \code{pairwise.htest} \cr
#'                                contains the calculated
#'                                \link[PMCMR]{posthoc.friedman.nemenyi.test}}
#' \item{cd.info}{[\code{list}] containing info on the critical difference
#'                  and its positioning}
#' \item{baseline}{\code{baseline} chosen for plotting}
#' \item{p.value}{p.value used for the \link[PMCMR]{posthoc.friedman.nemenyi.test}
#'                  and for computation of the critical difference}
#'
#' @family generate_plot_data
#' @family benchmark
#' @export
generateCritDifferencesData = function(bmr, measure = NULL, p.value = 0.05,
                                       baseline = NULL, test = "bd") {
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  assertChoice(test, c("nemenyi", "bd"))
  assertNumeric(p.value, lower = 0, upper = 1, len = 1)
  
  # Get Rankmatrix, transpose and get mean ranks
  transp.rankmat = as.data.frame(t(convertBMRToRankMatrix(bmr, measure)))
  mean.rank = apply(transp.rankmat, 2, mean)
  
  # Gather Info for plotting the descriptive part.
  df = data.frame(cbind(mean.rank),
                  learner.id = names(mean.rank),
                  rank = rank(mean.rank, ties.method = "average"))
  bst = df$rank < median(df$rank)
  df$yend[bst]  = subset(rank(df$rank, ties.method = "first"), bst) - 0.5
  df$yend[!bst] = subset(rank(desc(df$rank), ties.method = "first"), !bst) - 0.5
  df$xend  = 0
  df$xend[!bst]  = max(df$rank) + 1L
  df$xtext = 0
  df$xtext[!bst] = max(df$rank) + 1L
  
  # get a baseline
  if (is.null(baseline)) {
    baseline = df$learner.id[df$rank == min(df$rank)][1L]
  } else {
    assertChoice(baseline, getBMRLearnerIds(bmr))
  }
  # perform nemenyi test
  nem.test = friedmanPostHocTestBMR(bmr, measure, p.value)
  
  # info for plotting the cricital differences
  cd.info = list("test" = test,
                 "cd" = nem.test$crit.difference[[test]],
                 "x" = df$mean.rank[df$learner.id == baseline],
                 "y" = 0.1,
                 "barvjust" = 0,
                 "textvjust" = 0)
  
  # create data for the connecting bars
  if (test == "nemenyi") {
    sub = sort(df$mean.rank)
    mat = apply(t(outer(sub, sub, `-`)), c(1,2),
                FUN = function(x) ifelse(x > 0 && x < cd.info$cd, x, 0))
    xstart = round(apply(mat + sub, 1, min), 3)
    xend   = round(apply(mat + sub, 1, max), 3)
    nemdf = data.frame(xstart, xend, "diff" = xend - xstart)
    nemdf = ddply(nemdf, .(xend), function(x) x[which.max(x$diff), ])
    nemdf = nemdf[nemdf$xend - nemdf$xstart > 0, ]
    nemdf$y = seq(from = 0.1, to = 0.35, length.out = dim(nemdf)[1])
    cd.info$nemenyi.data = nemdf
  }
  
  makeS3Obj("critDifferencesData",
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
#' @param obj [\code{critDifferencesData}]
#'   Result of \link{generateCritDifferencesData} function.
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#'   Overwrites baseline from \link{generateCritDifferencesData}!\cr
#'   Select a [\code{learner.id} as baseline for the critical difference
#'   diagram, the critical difference will be positioned arround this learner.
#'   Defaults to best performing algorithm.
#' @template ret_gg2
#'
#' @references Janez Demsar, Statistical Comparisons of Classifiers over Multiple Data Sets,
#' JMLR, 2006
#'
#' @examples
#' lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
#' tasks = list(iris.task, sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' meas = list(acc, mmce)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' r = generateCritDifferencesData(res, mmce, p.value = 0.3, test = "bd")
#' plotCritDifferences(r)
#'
#' @family plot
#' @family benchmark
#' @export
plotCritDifferences = function(obj, baseline = NULL) {
  assertClass(obj, "critDifferencesData")
  
  p = ggplot(obj$data)
  p = p + geom_point(aes_string("mean.rank", 0, colour = "learner.id"), size = 3)
  p = p + geom_segment(aes_string("mean.rank", 0, xend = "mean.rank", yend = "yend",
                                  color = "learner.id"), size = 1)
  p = p + geom_segment(aes_string("mean.rank", "yend", xend = "xend",
                                  yend = "yend", color = "learner.id"), size = 1)
  p = p + geom_text(aes_string("xtext", "yend", label = "learner.id", color = "learner.id"),
                    vjust = -1)
  p = p + xlab("Average Rank")
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
  
  cd.x = obj$cd.info$x
  cd.y = obj$cd.info$y
  cd = obj$cd.info$cd
  
  # plot the critical difference bars
  if (obj$cd.info$test == "bd") {
    if (!is.null(baseline)) {
      assertChoice(baseline, obj$data$learner.id)
      obj$cd.info$x = obj$data$mean.rank[obj$data$learner.id == baseline][1]
    }
    
    p = p + annotate("segment", x = cd.x + cd, xend = cd.x - cd, y = cd.y, yend = cd.y,
                     alpha = 0.5, color = "darkgrey", size = 2)
    p = p + annotate("segment", x = cd.x + cd, xend = cd.x + cd, y = cd.y - 0.05,
                     yend = cd.y + 0.05, color = "darkgrey", size = 1)
    p = p + annotate("segment", x = cd.x - cd, xend = cd.x - cd, y = cd.y - 0.05,
                     yend = cd.y + 0.05, color = "darkgrey", size = 1, )
    p = p + annotate("point", x = cd.x, y = cd.y, alpha = 0.5)
    p = p + annotate("text", label = paste("Critical Difference =", round(cd, 2)),
                     x = cd.x, y = cd.y + 0.05, hjust = 0.5)
  } else {
    nemenyiData = obj$cd.info$nemenyi.data
    if (!(nrow(nemenyiData) == 0L)) {
      p = p + geom_segment(aes_string("xstart", "y", xend = "xend", yend = "y"), data = nemenyiData,
                           size = 2, color = "dimgrey", alpha = 0.9)
      p = p + annotate("text",
                       label = paste("Critical Difference =", round(cd, 2)),
                       y = max(obj$data$yend) + .1,
                       x = mean(obj$data$mean.rank),
                       hjust = 0.5)
      p = p + annotate("segment",
                       x =  mean(obj$data$mean.rank) - 0.5 * cd,
                       xend = mean(obj$data$mean.rank) + 0.5 * cd,
                       y = max(obj$data$yend) + .2,
                       yend = max(obj$data$yend) + .2,
                       size = 2L)
    }
  }
  return(p)
}
