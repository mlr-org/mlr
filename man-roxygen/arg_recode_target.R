#' @param recode.target [\code{character(1)}] \cr		+#' @template arg_subset
#'   Should target classes be recoded? Only for binary classification.		+#' @template arg_recode_target
#'   Possible are \dQuote{no} (do nothing), \dQuote{01}, and \dQuote{-1+1}.
#'   In the two latter cases the target vector is converted into a numeric vector.
#'   The positive class is coded as +1 and the negative class either as 0 or -1.
#'   Default is \dQuote{no}.
