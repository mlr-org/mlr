#' @param predict.threshold ([numeric])\cr
#'   Threshold to produce class labels. Has to be a named vector, where names correspond to class labels.
#'   Only for binary classification it can be a single numerical threshold for the positive class.
#'   See [setThreshold] for details on how it is applied.
#'   Default is `NULL` which means 0.5 / an equal threshold for each class.
#' @md
