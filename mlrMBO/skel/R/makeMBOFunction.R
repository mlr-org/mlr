#' Create fitness function for MBO.
#' 
#' Transforms a target/fitness function, which takes a vector as its first argument, into
#' a function which takes a list of values (as required by \code{\link{mbo}}.   
#'
#' @param fun [\code{function}]\cr 
#'   Fitness function.
#' @return Function which accepts decision values as a list.
#' @export 
makeMBOFunction = function(fun) {
  force(fun)
  function(x, ...) {
    fun(unlist(x), ...)
  }
}
