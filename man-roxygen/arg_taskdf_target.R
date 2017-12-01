#' @param target [\code{character(1)} | \code{character(2)} | \code{character(n.classes)}]\cr
#'   Name(s) of the target variable(s).
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#'   If survival analysis is applicable, these are the names of the survival time and event columns,
#'   so it has length 2.
#'   For multilabel classification these are the names of logical columns that indicate whether
#'   a class label is present and the number of target variables corresponds to the number of
#'   classes.
