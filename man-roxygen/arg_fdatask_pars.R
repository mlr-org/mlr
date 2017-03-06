#' @param fd.features [\code{list}]\cr Optional. Named list of column indices or
#'   column names. Each list entry defines one functional covariate through
#'   either a character vector of column names or an integer vector of column
#'   indices. The list names specify the names of the functional covariates. All
#'   columns that are not referenced in \code{fd.features} are scalar variables.
#'   Default is \dQuote{fd1} which means we assume that all columns form one
#'   functional covariate.
#' @param fd.grids [\code{list}]\cr Optional. Named list of grids over which the
#'   functional variables are observed. Each grid of observation points must be
#'   provided as a numerical vector. Default is \code{NULL} then all functional
#'   variables are observed on equidistant observation grids 1, 2, …, <number of
#'   observations per function>.
