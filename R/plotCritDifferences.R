#' @title Get Data for critical-differences Plot
#' 
#' @description Get a \code{data.frame} that can be used to plot a 
#' critical differences
#' plot. P-values and critical differences can be obtained from
#' \link{friedmanTestBMR} and \link{posthocNemenyiTestBMR}
#' 
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#'  
#' @return [\code{data.frame}] containing the meanRanks of all 
#' learners and infos on positioning of the descriptive lines.
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
#' @family generateData 
#' @export

 
generateCritDifferencesData = function(bmr, measure = NULL){
  
  #Assert correct inputs
  assertClass(bmr, "BenchmarkResult")
  if (is.null(measure))
    measure = getBMRMeasures(bmr)[[1L]]
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))  
  #Get Rankmatrix + Transpose
  Rmat = convertBMRToRankMatrix(bmr, measure)
  transRmat <- as.data.frame(t(Rmat[, -1L]))
  colnames(transRmat) <- Rmat[, 1L]
  # get MeanRanks
  meanRank = apply(transRmat, 2, mean)
  
  # gather Info for plotting the description
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
  return(df)  
}



#' @title plotCritDifferences
#' 
#' @description Plots a Critical-Difference Diagram for all classifiers and 
#' a selected measure. If a base is selected, the Critical Difference Interval
#' will be   
#' positioned arround the base. If not, the best performing algorithm will be 
#' chosen as base.
#' 
#' @details Credit: The output is a a critical differences plot, similar to 
#'  the one proposed in Demsar(2006). 
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#'  @param p.value [\code{numeric}(1)]\cr
#'  P-value for the critical difference. Default: 0.05
#'  @param base[\code{character(1)}]: [\code{learner.id} \cr
#'  Select a [\code{learner.id} as baseline for the critical difference
#'  diagram, the critical difference will be positioned arround this learner.
#'  Defaults to best performing algorithm.
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
#' plotCritDifferences(res, ber)
#' 
#' @family plot
#' @export

plotCritDifferences = function(bmr, measure=NULL, p.value = 0.05,base = NULL) {

  #Assert correct input
  if (is.null(measure)) {
     measure = getBMRMeasures(bmr)[[1L]]
  }
  assertClass(measure, "Measure")
  assertChoice(measure$id, getBMRMeasureIds(bmr))
  # Data
  df = generateCritDifferencesData(bmr, measure)
  #For baseline: if null choose best performing (random if two are equal)
  if (is.null(base)) {
     base = df$learner.id[df$rank == min(df$rank)]
  }
  assertChoice(base, getBMRLearnerIds(bmr))
  # NemenyiTest
  assertNumeric(p.value, lower = 0, upper = 1,len = 1)
  
  
  nemTest = posthocNemenyiTestBMR(bmr, measure, p.value)
  if (nemTest$fRejNull == FALSE) {
     message(c("Could not reject null hypothesis of friedman-test."))
  }
  # Info for Plotting the CD Interval
  CD = nemTest$cDifference
  CDx = df$meanRank[df$learner.id == base]
  CDy = 0.1
  
  p = ggplot(df, aes(color = learner.id)) + 
    geom_point(aes(x = meanRank, y = 0),size = 3) +
    geom_segment(aes(x = meanRank, xend = meanRank, y = 0, yend = yend),
                 size = 1) +
    geom_segment(aes(x = meanRank, xend = xend, y = yend, yend = yend),
                 size = 1) +
    geom_text(aes(x = xtext, y = yend, label = learner.id,
                  color = learner.id), vjust = -1 )+
    ylab("Average Rank") +
    scale_x_continuous(breaks = c(0:max(df$xend))) +
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
  
  #Plot Critical Difference Bar
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
