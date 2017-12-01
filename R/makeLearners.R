#' @title Create multiple learners at once.
#'
#' @description
#' Small helper function that can save some typing when creating mutiple learner objects.
#' Calls \code{\link{makeLearner}} multiple times internally.
#'
#' @param cls [\code{character}]\cr
#'   Classes of learners.
#' @param ids [\code{character}]\cr
#'   Id strings. Must be unique.
#'   Default is \code{cls}.
#' @param type [\code{character(1)}]\cr
#'   Shortcut to prepend type string to \code{cls} so one can set \code{cls = "rpart"}.
#'   Default is \code{NULL}, i.e., this is not used.
#' @inheritParams makeLearner
#' @return [named list of \code{\link{Learner}}]. Named by \code{ids}.
#' @family learner
#' @export
#' @examples
#' makeLearners(c("rpart", "lda"), type = "classif", predict.type = "prob")
makeLearners = function(cls, ids = NULL, type = NULL, ...) {
  if (!is.null(type)) {
    assertChoice(type, listTaskTypes())
    cls = stri_paste(type, cls, sep = ".")
  }
  assertCharacter(cls, any.missing = FALSE)
  assertCharacter(ids, any.missing = FALSE, len = length(cls), unique = TRUE, null.ok = TRUE)
  ids = coalesce(ids, cls)
  # args check are all done by makeLearner
  lrns = mapply(makeLearner, cl = cls, id = ids, MoreArgs = list(...), SIMPLIFY = FALSE)
  setNames(lrns, ids)
}


