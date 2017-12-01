#' Crossover.
#'
#' Takes two bit strings and creates a new one of the same size by selecting the items from the first string or
#' the second, based on a given rate (the probability of choosing an element from the first string).
#'
#' @param x [\code{logical}]\cr
#'   First parent string.
#' @param y [\code{logical}]\cr
#'   Second parent string.
#' @param rate [\code{numeric(1)}]\cr
#'   A number representing the probability of selecting an element of the first string.
#'   Default is \code{0.5}.
#' @return [\code{\link{crossover}}].
#' @name crossover
#' @rdname crossover
#' @aliases crossover
NULL

crossover = function(x, y, rate = 0.5) {
  ratio = rbinom(length(x), 1, rate)
  ifelse(ratio == 1, x, y)
}
