#' @title Generate data for critical-differences plot.
#' 
#' @description Generate data that can be used to plot a 
#'  critical differences plot.
#' 
#' @return [\code{critDifferencesData}], containing: \cr
#'  $\code{$data}: [\code{data.frame}] containing the info for the descriptive
#'                part of the plot.\cr 
#'  $\code{friedman.nemenyi.test}: [\code{list}] of class \code{pairwise.htest}
#'  \cr
#'                                \link[PMCMR]{friedman.nemenyi.test}
#'                                calculated. \cr
#'  $\code{cd.info}: [\code{list}] containing info on the critical difference
#'                  and its positioning.\cr
#'  $\code{baseline}: \code{baseline} chosen for plotting.\cr
#'  $\code{p.value}: p.value used for the \link[PMCMR]{friedman.nemenyi.test}
#'                  and for computation of the \code{Critical Difference}.
#' 
#' 
#' @details Computes the critical differences according to either the 
#'  \code{"Bonferroni-Dunn"} method or the \code{"Nemenyi"} method.\cr  
#'  \code{"Bonferroni-Dunn"} usually yields higher power as it does not 
#'  compare all algorithms to each other, but all algorithms to a 
#'  \code{baseline} instead.
#' @param bmr [\code{\link{BenchmarkResult}}] \cr
#'  Output of a \code{\link{benchmark}} function.
#' @param measure [\code{\link{Measure}}] \cr
#'  Measure for which ranks should be calculated (e.g: acc). 
#'  Defaults to first.
#' @param p.value [\code{numeric}(1)]\cr
#'  P-value for the critical difference. Default: 0.05
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#'  Select a [\code{learner.id} as baseline for the \code{test = "bd"}
#'  ("Bonferroni-Dunn") critical differences
#'  diagram.The critical difference Interval will then be positioned arround this learner.
#'  Defaults to best performing algorithm. \cr
#'  For \code{test = "nemenyi"}, no baseline is needed as it performs \code{all pairwise
#'  comparisons.} 
#' @param test [\code{character(1)}] \cr
#'  Test for which the critical differences are computed. \cr
#'  [\code{"bd"}] for the Bonferroni-Dunn Test, which is comparing all
#'  classifiers to a \code{baseline}, thus performing a comparison
#'  of one classifier to all others. \cr
#'  Algorithms not connected by a single line are statistically different(better or worse)
#'  then the baseline. \cr
#'  [\code{"nemenyi"}] for the \code{\link[PMCMR]{posthoc.friedman.nemenyi.test}}
#'  which is comparing all classifiers to each other. The null hypothesis that 
#'  there is a difference between the classifiers can not be rejected for all
#'  classifiers that have a single grey bar connecting them.
#'  
#' 
#' @examples 
#' # see plotCritDifferences
#' 
#' @family generate_plot_data , benchmark
#' @export

 
generateCritDifferencesData = function(bmr, measure = NULL, p.value = 0.05,
                                       baseline = NULL, test = "bd") {
  
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr)) 
  assertChoice(test, c("nemenyi","bd"))
  assertNumeric(p.value, lower = 0, upper = 1, len = 1)
  
  #Get Rankmatrix, transpose and get mean ranks
  rankmat = convertBMRToRankMatrix(bmr, measure)
  transp.rankmat <- as.data.frame(t(rankmat))
  mean.rank = apply(transp.rankmat, 2, mean)
  
  # Gather Info for plotting the descriptive part.
  n.learners = length(getBMRLearnerIds(bmr))
  df = data.frame(cbind(mean.rank),
                  learner.id = names(mean.rank),
                  rank = rank(mean.rank, ties.method = "average"))
  bst = df$rank < mean(df$rank)
  df$yend[bst]  = subset(df$rank, bst) - 0.5
  df$yend[!bst] = subset(rank(desc(df$rank)), !bst) - 0.5
  df$xend  = 0
  df$xend[!bst]  = max(df$rank) + 1
  df$xtext = 0
  df$xtext[!bst] = max(df$rank) + 1
  
  # get a baseline
  if(is.null(baseline))
    baseline = df$learner.id[df$rank == min(df$rank)]
  
  # perform nemenyi test
  nem.test = friedmanPostHocTestBMR(bmr, measure, p.value)
  
  # info for plotting the cricital differences
  cd.info = list("test" = test,
                "cd" = nem.test$crit.difference[[test]],
                "x" = df$mean.rank[df$learner.id == baseline],
                "y" = 0.1)
  
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
            
  out = list("data" = df,
             "cd.info" = cd.info,
             "friedman.nemenyi.test" = nem.test,
             "baseline" = baseline,
             "p.value" = p.value)
  
  
  class(out) = append(class(out), "critDifferencesData")
  return(out)
 
}



#' @title Plot critical differences for a selected measure.
#' 
#' @description Plots a critical-differences diagram for all classifiers and 
#' a selected measure. If a baseline is selected, the critical difference
#' interval will be positioned arround the baseline. If not, the best
#' performing algorithm will be chosen as baseline.
#' 
#' @return [\link{ggplot2}] plot
#' 
#' @details Credit: The output is a a critical differences plot, similar to 
#'  the one proposed in Demsar(2006). 
#' 
#' @param obj [\code{critDifferencesData}]
#'  Result of \link{generateCritDifferencesData} function.
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#'  Overwrites baseline from \link{generateCritDifferencesData}!\cr
#'  Select a [\code{learner.id} as baseline for the critical difference
#'  diagram, the critical difference will be positioned arround this learner.
#'  Defaults to best performing algorithm.
#'  
#' 
#' @examples 
#' # see 
#' g = generateCritDifferencesData(res,acc, p.value = 0.1, test = "nemenyi")
#' plotCritDifferencesData(g)
#' 
#' @family plot, generate_plot_data, benchmark
#' @export

plotCritDifferences = function(obj, baseline = NULL) {

  assertClass(obj, "critDifferencesData")
  if (!is.null(baseline)) {
    assertChoice(baseline, obj$data$learner.id)
  } else {
    baseline =obj$baseline
  }
    

  # plot descriptive part
  p = ggplot(obj$data) + 
    geom_point(aes(x = mean.rank, y = 0, color = learner.id), size = 3) +
    geom_segment(aes(x = mean.rank, xend = mean.rank, y = 0, yend = yend,
                     color = learner.id), size = 1) +
    geom_segment(aes(x = mean.rank, xend = xend, y = yend, yend = yend,
                     color = learner.id), size = 1) +
    geom_text(aes(x = xtext, y = yend, label = learner.id,
                  color = learner.id), vjust = -1) +
    ylab("Average Rank") +
    scale_x_continuous(breaks = c(0:max(obj$data$xend))) +
    theme(axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line    = element_line(size = 1), 
          axis.line.y  = element_blank(),
          panel.grid.major = element_blank(), 
          plot.background  = element_blank())
  
  cd.x = obj$cd.info$x
  cd.y = obj$cd.info$y
  cd = obj$cd.info$cd
 
  
  # plot the critical difference bars  
  if (obj$cd.info$test == "bd") {
  p = p + 
    annotate("segment", x = cd.x + cd, xend = cd.x - cd, y = cd.y,
             yend = cd.y, alpha = 0.5, color = "darkgrey", size = 2) +
    annotate("segment", x = cd.x + cd, xend = cd.x + cd, y = cd.y - 0.05,
             yend = cd.y + 0.05, color = "darkgrey", size = 1) +
    annotate("segment", x = cd.x - cd, xend = cd.x - cd, y = cd.y - 0.05,
             yend = cd.y + 0.05, color = "darkgrey", size = 1) +
    annotate("point", x = cd.x, y = cd.y, alpha = 0.5) +
    annotate("text", label = paste("Critical Difference =", round(cd,2)),
             x = cd.x, y = cd.y, vjust = -1)
  
  } else if (obj$cd.info$test == "nemenyi") {
    nemenyiData = obj$cd.info$nemenyi.data
    p = p +
      geom_segment(aes(x = xstart - .03, xend = xend + .03, y = y, yend = y),
                   data = nemenyiData, size = 2, color = "dimgrey", alpha = 0.9) + 
      annotate("text", label = paste("Critical Difference =", round(cd,2)),
               y = max(obj$data$yend), x = mean(obj$data$mean.rank), vjust = -1) + 
      annotate("segment",
               x =  mean(obj$data$mean.rank) - 0.5 * cd, 
               xend = mean(obj$data$mean.rank) + 0.5 * cd,
               y = max(obj$data$yend) + .2,
               yend = max(obj$data$yend) + .2,
               size = 2)
  }
  return(p)
} 

