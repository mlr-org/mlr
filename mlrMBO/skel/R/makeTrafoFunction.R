#' Create transformation function for MBOExampleRun.
#'    
#' @param fun [\code{character(1)}]\cr 
#'   Name of the transformation.
#' @param fun [\code{function}]\cr
#'   R function which expects a numeric vector.
#' @return Object of type MBOTrafoFunction.
#' @export 
makeTrafoFunction = function(name, fun) {
    structure(
        fun,
        name=name,
        class=c("MBOTrafoFunction", class(fun)))
}

#' @export
logTrafo = function() {
    makeTrafoFunction(
        name = "log transformation",
        fun = function(x) {
            if (any(x < 0)) {
                #FIXME what about this?
                warning("Negative function values. Shifting function to apply logarithm.")
                x = x - min(x) + 1
            }
            return(log(x))
        })
}