#' @param id (`character(1)`)\cr
#'   Id string for object.
#'   Default is the name of the R variable passed to `data`.
#' @param data ([data.frame])\cr
#'   A data frame containing the features and target variable(s).
#' @param target (`character(1)` | `character(2)` | `character(n.classes)`)\cr
#'   Name(s) of the target variable(s).
#'   For survival analysis these are the names of the survival time and event columns,
#'   so it has length 2. For multilabel classification it contains the names of the logical
#'   columns that encode whether a label is present or not and its length corresponds to the
#'   number of classes.
#' @param weights ([numeric])\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   Cannot be set for cost-sensitive learning.
#'   Default is `NULL` which means no (= equal) weights.
#' @param blocking ([factor])\cr
#'   An optional factor of the same length as the number of observations.
#'   Observations with the same blocking level \dQuote{belong together}.
#'   Specifically, they are either put all in the training or the test set
#'   during a resampling iteration.
#'   Default is `NULL` which means no blocking.
#' @param positive (`character(1)`)\cr
#'   Positive class for binary classification (otherwise ignored and set to NA).
#'   Default is the first factor level of the target attribute.
#' @param fixup.data (`character(1)`)\cr
#'   Should some basic cleaning up of data be performed?
#'   Currently this means removing empty factor levels for the columns.
#'   Possible choices are:
#'   \dQuote{no} = Don't do it.
#'   \dQuote{warn} = Do it but warn about it.
#'   \dQuote{quiet} = Do it but keep silent.
#'   Default is \dQuote{warn}.
#' @param check.data (`logical(1)`)\cr
#'   Should sanity of data be checked initially at task creation?
#'   You should have good reasons to turn this off (one might be speed).
#'   Default is `TRUE`.
#' @param coordinates ([data.frame])\cr
#'   Coordinates of a spatial data set that will be used for spatial partitioning of the data in a spatial cross-validation resampling setting.
#'   Coordinates have to be numeric values.
#'   Provided [data.frame] needs to have the same number of rows as data and consist of at least two dimensions.
