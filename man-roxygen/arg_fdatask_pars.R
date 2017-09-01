#' @param fd.features [Named \code{list} of \code{character} or \code{integer}]\cr
#'   Optional. Functional features, as column indices or column names of the passed data.
#'   The list names specify the names of the functional features.
#'   Each list entry defines one functional feature through column names or indices.
#'   Unreferenced columns are assumed to be scalar features.
#'   Default is \code{NULL}, then we assume all columns to form one functional feature called \dQuote{fd1}.
#' @param fd.grids [\code{list} of \code{numeric}]\cr
#'   Optional. Grids over which the functional features are observed.
#'   Default is \code{NULL}, then all functional
#'   features are observed on equidistant observation grids 1, 2, …, <number of
#'   observations per function>.
