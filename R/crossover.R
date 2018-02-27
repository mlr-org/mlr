#' Crossover.
#'
#' Takes two bit strings and creates a new one of the same size by selecting the items from the first string or
#' the second, based on a given rate (the probability of choosing an element from the first string).
#'
#' @param x ([logical])\cr
#'   First parent string.
#' @param y ([logical])\cr
#'   Second parent string.
#' @param rate (`numeric(1)`)\cr
#'   A number representing the probability of selecting an element of the first string.
#'   Default is `0.5`.
#' @return ([crossover]).
#' @name crossover
#' @rdname crossover
#' @aliases crossover
NULL

crossover = function(x, y, rate = 0.5) {
  ratio = rbinom(length(x), 1, rate)
  ifelse(ratio == 1, x, y)
}
