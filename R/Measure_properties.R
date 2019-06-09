#' @title Query properties of measures.
#'
#' @description
#' Properties can be accessed with `getMeasureProperties(measure)`, which returns a
#' character vector.
#'
#' The measure properties are defined in [Measure].
#' @template arg_measure
#' @param props ([character])\cr
#'   Vector of properties to query.
#' @return `getMeasureProperties` returns a character vector with measure properties.
#'  `hasMeasureProperties` returns a logical vector of the same length as `props`.
#' @name MeasureProperties
#' @rdname MeasureProperties
#' @aliases getMeasureProperties hasMeasureProperties
NULL

#' @rdname MeasureProperties
#' @export
getMeasureProperties = function(measure) {
  assertClass(measure, classes = "Measure")
  measure$properties
}

#' @rdname MeasureProperties
#' @export
hasMeasureProperties = function(measure, props) {
  assertClass(measure, classes = "Measure")
  assertSubset(props, listMeasureProperties())
  props %in% getMeasureProperties(measure)
}

#' @title List the supported measure properties.
#'
#' @description
#' This is useful for determining which measure properties are available.
#'
#' @return ([character]).
#'
#' @export
listMeasureProperties = function() {
  mlr$measure.properties
}
