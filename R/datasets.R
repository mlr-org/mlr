#' Iris classification task
#'
#' Contains the following objects:
#' \describe{
#'  \item{iris.task}{Classification task.}
#'  \item{iris.lrn}{Classification learner using \code{classif.lda}.}
#'  \item{iris.train}{Training subset of \code{iris.task}.}
#'  \item{iris.test}{Test subset of \code{iris.task}.}
#' }
#'
#' @name mlr.iris
#' @aliases iris.task iris.lrn iris.train iris.test
#' @references See \code{\link[datasets]{iris}}.
#' @keywords data
#' @docType data
NULL

#' Sonar classification task
#'
#' List with following named elements:
#' \describe{
#'  \item{sonar.task}{Classification task.}
#'  \item{sonar.lrn}{Classification learner using \code{classif.rpart}.}
#'  \item{sonar.train}{Training subset of \code{sonar.task}.}
#'  \item{sonar.test}{Test subset of \code{sonar.task}.}
#' }
#'
#' @name mlr.sonar
#' @aliases sonar.task sonar.lrn sonar.train sonar.test
#' @references See \code{\link[mlbench]{Sonar}}.
#' @keywords data
#' @docType data
NULL

#' BostonHousing regression task
#'
#' Contains the following objects:
#' \describe{
#'  \item{bh.task}{Regression task.}
#'  \item{bh.lrn}{Classification learner using \code{regr.lm}.}
#'  \item{bh.train}{Training subset of \code{bh.task}.}
#'  \item{bh.test}{Test subset of \code{bh.task}.}
#' }
#'
#' @name mlr.bh
#' @aliases bh.task bh.lrn bh.train bh.test
#' @references See \code{\link[mlbench]{BostonHousing}}.
#' @keywords data
#' @docType data
NULL

#' Wisonsin Prognostic Breast Cancer (WPBC) survival task
#'
#' Contains the following objects:
#' \describe{
#'  \item{wpbc.task}{Regression task.}
#'  \item{wpbc.lrn}{Classification learner using \code{regr.lm}.}
#'  \item{wpbc.train}{Training subset of \code{wpbc.task}.}
#'  \item{wpbc.test}{Test subset of \code{wpbc.task}.}
#' }
#'
#' @name mlr.wpbc
#' @aliases wpbc.task wpbc.lrn wpbc.train wpbc.test
#' @references See \code{\link[mboost]{wpbc}}. Four incomplete cases have been
#'   removed from the task.
#' @keywords data
#' @docType data
NULL
