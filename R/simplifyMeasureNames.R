#' @title Simplify measure names.
#'
#' @description
#' Clips aggregation names from character vector.
#' E.g: 'mmce.test.mean' becomes 'mmce'.
#' Elements that don't contain a measure name are ignored and returned unchanged.
#'
#' @param xs ([character])\cr
#'   Character vector that (possibly) contains aggregated measure names.
#' @return ([character]).
#' @export
simplifyMeasureNames = function(xs) {

  assertCharacter(xs, any.missing = FALSE)
  # get all measure names
  all.measure.names = listMeasures()
  # cut everything after and including the first '.'
  xs.shortened = stri_replace_all_regex(xs, "\\..*", "")
  # check if this is a measure
  string.is.measure = (xs.shortened %in% all.measure.names)
  # if yes: insert shortened name, else insert original input
  res = ifelse(string.is.measure, xs.shortened, xs)
  as.character(res)
}
