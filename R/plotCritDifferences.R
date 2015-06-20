#' @title Generate Data for critical-differences Plot
#' 
#' @description Generate data that can be used to plot a 
#' critical differences plot.
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  \link{Measure} for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#' @param p.value [\code{numeric}(1)]\cr
#'  P-value for the critical difference. Default: 0.05
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#' Select a [\code{learner.id} as baseline for the critical difference
#' diagram, the critical difference will be positioned arround this learner.
#' Defaults to best performing algorithm.
#'  
#' @return [\code{critDifferencesData}], containing: \cr
#' $\code{$data}: [\code{data.frame}] containing the info for the descriptive
#'                part of the plot.\cr 
#' $\code{friedman.nemenyi.test}: [\code{list}] of class \code{pairwise.htest}
#'  \cr
#'                                \link[PMCMR]{friedman.nemenyi.test}
#'                                calculated. \cr
#' $\code{cdInfo}: [\code{list}] containing info on the critical difference
#'                  and its positioning.\cr
#' $\code{baseline}: \code{baseline} chosen for plotting.\cr
#' $\code{p.value}: p.value used for the \link[PMCMR]{friedman.nemenyi.test}
#'                  and for computation of the \code{Critical Difference}.
#' 
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.nnet"),
#'             makeLearner("classif.rpart"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5L)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' generateCritDifferencesData(res, acc)
#' 
#' @family generate_plot_data , critDifference
#' @export

 
generateCritDifferencesData = function(bmr, measure = NULL, p.value = 0.05,
                                       baseline = NULL) {
  
  #Assert correct inputs
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr)) 
  
  #Get Rankmatrix, Transpose and get mean ranks
  Rmat = convertBMRToRankMatrix(bmr, measure)
  transRmat <- as.data.frame(t(Rmat[, -1L]))
  colnames(transRmat) <- Rmat[, 1L]
  meanRank = apply(transRmat, 2, mean)
  
  # Gather Info for plotting the descriptive part.
  nLearners = length(getBMRLearnerIds(bmr))
  df = data.frame(cbind(meanRank),
                  learner.id = names(meanRank),
                  rank = rank(meanRank, ties.method = "random"))
  bst = df$rank < mean(df$rank)
  df$yend[bst]  = subset(df$rank, bst) - 0.5
  df$yend[!bst] = subset(rank(desc(df$rank)), !bst) - 0.5
  df$xend  = 0
  df$xend[!bst]  = max(df$rank) + 1
  df$xtext = 0
  df$xtext[!bst] = max(df$rank) + 1
  
  #Baseline
  
  if(is.null(baseline))
    baseline = df$learner.id[df$rank == min(df$rank)]
  assertChoice(baseline, getBMRLearnerIds(bmr))
  
  # NemenyiTest
  assertNumeric(p.value, lower = 0, upper = 1,len = 1)
  nemTest = posthocNemenyiTestBMR(bmr, measure, p.value)
  if (nemTest$fRejNull == FALSE)
    message(c("Could not reject null hypothesis of friedman-test."))

  # Info for Plotting the CD Interval
  cdInfo = list("cd" = nemTest$cDifference,
                "x" = df$meanRank[df$learner.id == baseline],
                "y" = 0.1)
  
  # Output
  out = list("data" = df,
             "cdInfo" = cdInfo,
             "friedman.nemenyi.test" = nemTest,
             "baseline" = baseline,
             "p.value" = p.value)
  
  
  class(out) = append(class(out), "critDifferencesData")
  return(out)
 
}



#' @title plotCritDifferences
#' 
#' @description Plots a Critical-Differences Diagram for all classifiers and 
#' a selected measure. If a baseline is selected, the Critical Difference
#' Interval will be positioned arround the baseline. If not, the best
#' performing algorithm will be chosen as baseline.
#' 
#' @details Credit: The output is a a critical differences plot, similar to 
#'  the one proposed in Demsar(2006). 
#' 
#' @param obj [\code{critDifferencesData}]
#' Result of \link{generateCritDifferencesData} function.
#' @param baseline [\code{character(1)}]: [\code{learner.id}] \cr
#' Overwrites baseline from \link{generateCritDifferencesData}!\cr
#' Select a [\code{learner.id} as baseline for the critical difference
#' diagram, the critical difference will be positioned arround this learner.
#' Defaults to best performing algorithm.
#'  
#' @return [\link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.nnet"),
#'             makeLearner("classif.rpart"), makeLearner("classif.svm"))
#' tasks = list(iris.task, sonar.task, pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5L)
#' meas = list(acc, mmce, ber, featperc)
#' res = benchmark(lrns, tasks, rdesc, meas)
#' g = generateCritDifferencesData(res,acc, p.value = 0.1)
#' plotCritDifferencesData(p)
#' 
#' @family plot, critDifference
#' @export

plotCritDifferences = function(obj, baseline = NULL) {

  #Assert correct input
  assertClass(obj, "critDifferencesData")
  # Data
  #For baseline: if null choose best performing (random if two are equal)
  if (!is.null(baseline)) {
    assertChoice(baseline, getBMRLearnerIds(bmr))
    obj$baseline = baseline
  }
  
  
  
  p = ggplot(obj$data, aes(color = learner.id)) + 
    geom_point(aes(x = meanRank, y = 0),size = 3) +
    geom_segment(aes(x = meanRank, xend = meanRank, y = 0, yend = yend),
                 size = 1) +
    geom_segment(aes(x = meanRank, xend = xend, y = yend, yend = yend),
                 size = 1) +
    geom_text(aes(x = xtext, y = yend, label = learner.id,
                  color = learner.id), vjust = -1 )+
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
  
  CDx = obj$cdInfo$x
  CDy = obj$cdInfo$y
  CD = obj$cdInfo$cd
  # Plot Critical Difference Bar
  
  p = p + 
    annotate("segment", x = CDx + CD, xend = CDx - CD, y = CDy,
             yend = CDy, alpha = 0.5, color = "darkgrey", size = 2) +
    annotate("segment", x = CDx + CD, xend = CDx + CD, y = CDy - 0.05,
             yend = CDy + 0.05, color = "darkgrey", size = 1) +
    annotate("segment", x = CDx - CD, xend = CDx - CD, y = CDy - 0.05,
             yend = CDy + 0.05, color = "darkgrey", size = 1) +
    annotate("point", x = CDx, y = CDy, alpha = 0.5) +
    annotate("text", label = paste("Critical Difference =", round(CD,2)),
             x = CDx, y = CDy, vjust = -1)
  return(p)
} 
