##########################################################################
#' Get Data for critical-differences Plot
#' 
#' Get a \code{data.frame} that can be used to plot a critical differences
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
#' lrns = list(makeLearner("classif.randomForest"),makeLearner("classif.rpart"),
#'             makeLearner("classif.nnet"), makeLearner("classif.svm"))
#' tasks = list(iris.task,sonar.task,pid.task)
#' rdesc = makeResampleDesc("CV", iters = 5)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' plotCritDifferences(res,acc,0.05,"classif.svm")
#' 
#' @export
 
getCritDifferencesData = function(bmr,measure){
  #Assert correct inputs
  assertClass(bmr, "BenchmarkResult")
  if (!is.null(measure)){
    assertClass(measure, "Measure")
  } else {
    measure = getBMRMeasures(bmr)[[1]]
  }
  #Get Rankmatrix
  Rmat = convertBMRToRankMatrix(bmr,measure)
  # transpose RankMatrix
  transRmat <- as.data.frame(t(Rmat[,-1L]))
  colnames(transRmat) <- Rmat[,1L]
  # get MeanRanks
  meanRank = apply(transRmat,2,mean)
  # get Info for Plotting CD
  nLearners = length(Rmat$learner.id)
  df = data.frame(cbind(meanRank),
                  learner.id = names(meanRank),
                  rank = rank(meanRank,ties.method = "random"))
  bst = df$rank < nLearners/2
  df$yend[bst]  = subset(df$rank,bst)-0.5
  df$yend[!bst] = subset(rank(desc(df$rank)),!bst)-0.5
  df$xend  = 0L
  df$xend[!bst]  = max(df$rank)+1
  df$xtext = 0L
  df$xtext[!bst] = max(df$rank)+1  
  return(df)  
}





##########################################################################
#' plotCritDifferences
#' 
#' Plots a Critical-Difference Diagram for all classifiers and a selected
#' measure. If a base is selected, the Critical Difference Interval will be   
#' positioned arround the base. If not, the best performing algorithm will be 
#' chosen as base. 
#' 
#' @param bmr \link[mlr]{BenchmarkResult}\cr
#'  Output of a \link[mlr]{benchmark} function.
#' @param measure \link[mlr]{Measure} \cr
#'  Measure for which ranks should be calculated (e.g: acc).
#'  Defaults to first. 
#'  @param p.value [\code{numeric}(1)]\cr
#'  P-value for the critical difference. Default: 0.05
#'  @param base[\code{character(1)}]: learner.id \cr
#'  Select a learner.id as baseline for the critical difference diagram,
#'  the critical difference will be positioned arround this learner.
#'  Defaults to best performing algorithm.
#'  
#' @return [\link{ggplot2}] plot
#' 
#' @examples 
#' lrns = list(makeLearner("classif.lda"),makeLearner("classif.rpart"))
#' tasks = list(iris.task,sonar.task)
#' rdesc = makeResampleDesc("CV", iters = 3)
#' meas = list(acc,mmce,ber,featperc)
#' res = benchmark(lrns, tasks, rdesc,meas)
#' plotCritDifferences(res,acc,0.05,"classif.lda")
#' 
#' @export

plotCritDifferences = function(bmr,measure=NULL,p.value= 0.05,base = NULL){
  # Correct input assertion is done in getData
  # Data
  df = getCritDifferencesData(bmr,measure)
  #For baseline: if null choose best performing (random if two are equal)
  if (is.null(base)){
    base = df$learner.id[df$rank == min(df$rank)]
  }
  # NemenyiTest
  assertNumeric(p.value,lower = 0, upper = 1,len=1)
  nemTest = posthocNemenyiTestBMR(bmr,measure,p.value)
  if (nemTest$fRejNull == FALSE){
    message(c("Could not reject null hypothesis of friedman-test."))
  }
  CD = nemTest$cDifference
  CDx = df$meanRank[df$learner.id == base]
  CDy = 0.1
  p = ggplot(df, aes(color = learner.id))+ 
    geom_point(aes(x = meanRank, y= 0))+
    geom_segment(aes(x = meanRank,xend = meanRank, y=0,yend=yend),
                 size = 1)+
    geom_segment(aes(x = meanRank,xend = xend,y=yend, yend=yend),
                 size = 1)+
    geom_text(aes(x=xtext, y=yend,label = learner.id,
                  color=learner.id), vjust=-1 )+
    ylab("Average Rank")+
    scale_x_continuous(breaks=c(0:max(df$xend)))+
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
  p = p + annotate("segment", x=CDx +CD, xend = CDx-CD, y = CDy,
                   yend = CDy,
                   alpha = 0.5, color = "darkgrey",size = 2) +
    annotate("segment", x=CDx +CD, xend = CDx+CD, y =CDy-0.05,
             yend = CDy+0.05, color = "darkgrey",size = 1) +
    annotate("segment", x=CDx -CD, xend = CDx-CD, y =CDy-0.05,
             yend = CDy+0.05, color = "darkgrey",size = 1)+
    annotate("point", x=CDx,y=CDy,alpha = 0.5)+
    annotate("text",label = 
               paste("Critical Difference =",round(CD,2)),
             x = CDx, y = CDy, vjust = -1)
  return(p)
} 
