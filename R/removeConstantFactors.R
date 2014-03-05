#' Remove constant factors from a data set.
#' 
#' Constant factors can lead to errors in some models. Given a \code{data.frame}, this function
#' removes all constant factors. With the argument 'perc', there is a possibility to also remove
#' factors for whom less than 'perc' percent of the observations differ from the mode value.
#' 
#' @param df [\code{\link{data.frame}}]\cr
#'   The data set.
#' @param df [\code{numeric}]\cr
#'   The percentage of a factor's values (in [0, 100]) that must differ from the mode value. 
#'   Default is 0, which means only constant factors with exactly one observed level are removed. 

removeConstantFactors = function(df, perc=0) {
  checkArg(df, "data.frame")
  checkArg(perc, "numeric", lower=0, upper=100)
  
  if (ncol(df) == 0) 
    return(df)
  
  factor.inds = which(unlist(lapply(df, is.factor)))
  remove.inds = c()
  for(i in factor.inds) {
    if(nlevels(df[, i]) == 1) {
      remove.inds = c(remove.inds, i)
    } else {
      # check if less than 'perc'% of a factor's values differ from the mode value
      # if so, remove this factor.
      if (sum(sort(table(df[, i]), decreasing = TRUE)[-1])/length(df[, i]) < perc/100) {
        remove.inds = c(remove.inds, i)
      }
    }  
  }
  if(length(remove.inds) > 0) {
    messagef("Removing %i constant column(s)...", length(remove.inds))
    df = df[-remove.inds]
  }
  return(df)
}