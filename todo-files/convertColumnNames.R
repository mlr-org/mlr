#' Removes specials characters in column names.
#'
#' Currently the default bevaviour is: 
#' The following special character are eliminated and converted via
#' utf8ToInt to a number. 
#' [ ] { } ( ) , + - * / = $ ~
#' 
#' @param data [\code{data.frame}]\cr 
#'   Data to convert.
#' @return [\code{data.frame}]
#' 
#' @export
#' @title Removes specials characters in column names.

convertColumnNames = function(data) {
  replace = c("\\[", "]", "\\(", ")", "\\{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
 
#  if (missing(replace)) {
#    #todo: these are all candidates for bad chars
#    #! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
#    replace = c("\\[", "]", "\\(", ")", "\\{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
#  } else {
#    checkArg(replace, "character")
#    if(!all(sapply(names(replace), function(x) nchar(x) == 1)))
#      stop("All names of 'replace' have to be single characters!")
#  }
  #replace.collapsed = paste(sapply(replace, function(x) substr(x, nchar(x), nchar(x))), collapse=" ")
  
  cns = colnames(data)
  for (bc in replace) {
    # take last int code when escaping regexp
    cns = gsub(pattern=bc, replacement=rev(utf8ToInt(bc))[1], cns)
  }
  colnames(data) = cns
  return(data)
}
  