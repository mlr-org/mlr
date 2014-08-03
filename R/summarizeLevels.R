#' Summarizes factors of a data.frame by tabling them.
#'
#' @param data [\code{data.frame}]\cr 
#'   Data to summarize. 
#'   Characters and logicals will be treated as factors.   
#' @param which [\code{character}]\cr
#'   Restrict result to columns in \code{which}. 
#'   Default is all factor, character and logical columns of \code{data}.   
#' @return A named list of tables.
#' 
#' @export
#' @title Summarize factors of a data.frame.

summarizeLevels = function(data, which) {
  n = ncol(data)
  cns = colnames(data)
  res = list()
  pred = function(x) is.factor(x) || is.logical(x) || is.character(x)    
  if (missing(which)) 
    which = Filter(function(x) pred(data[,x]), cns)
  else
    if (!all(which %in% cns)) 
      stop("Undefined columns selected!")
  for (x in which) {
    if (!pred(data[,x]))
      stop(x, " is not a factor, logical or character!")
     res[[x]] = table(as.factor(data[,x]))
  }
  return(res)
}
