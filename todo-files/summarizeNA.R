#' Characterizes only variables of a data set with missing values. So, missing values are painted
#' black, while other observations keep white.
#'
#' @param data [\code{data.frame}]\cr
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor or character.
#'   Characters and logicals will be treated as factors.
#' @return Names of the variables with their frequency of missing values and an additional plot
#'   which shows the position of the missing values (color = black) for each variable with NAs.
#'
#' @export
#' @title Giving an image of a dataset with missing values


summaryNA  <- function(dataset,show.plot=F,margin.left=4){

  num <- as.numeric(which(apply(is.na(dataset),2,any)))

  if(length(num) > 0){

    cat("Variables with NAs: ",colnames(dataset)[num],"\n")
    cat("Number of NAs: ",colSums(is.na(dataset[,num,drop=F])),"\n")

    dataset.new <- dataset[,num,drop=F]
    color <- apply(dataset.new, 2, function(x) as.integer(is.na(x)))

    if(show.plot){

      image(color,col=c("white","black"),yaxt="n")
      par(mar=c(5, margin.left, 4, 2) + 0.1)
      abline(v=-0.001)
      abline(h=1.015)

      if(length(num) == 1){
        y.type <- 0
      } else {
        y.type <- 0:(ncol(dataset.new)-1)/(length(dataset.new)-1)
      }

      axis(2, labels=colnames(dataset.new), at=y.type, las=2)
   }
  }

  else{
    cat("There are no missing values in this dataset!!!", "\n")
  }
}
