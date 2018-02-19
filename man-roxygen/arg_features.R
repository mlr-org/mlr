#' @param features ([character] | [integer] | [logical])\cr
#'   Vector of selected inputs. You can either pass a character vector with the
#'   feature names, a vector of indices, or a logical vector.\cr
#'   In case of an index vector each element denotes the position of the feature
#'   name returned by [getTaskFeatureNames].\cr
#'   Note that the target feature is always included in the
#'   resulting task, you should not pass it here.
#'   Default is to use all features.
#' @md
