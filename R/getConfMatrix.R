#' @title Confusion matrix.
#'
#' @description
#' Calculates confusion matrix for (possibly resampled) prediction.
#' Rows indicate true classes, columns predicted classes.
#'
#' The marginal elements count the number of classification
#' errors for the respective row or column, i.e., the number of errors
#' when you condition on the corresponding true (rows) or predicted
#' (columns) class. The last element in the margin diagonal
#' displays the total amount of errors.
#'
#' Note that for resampling no further aggregation is currently performed.
#' All predictions on all test sets are joined to a vector yhat, as are all labels
#' joined to a vector y. Then yhat is simply tabulated vs y, as if both were computed on
#' a single test set. This probably mainly makes sense when cross-validation is used for resampling.
#'
#' @template arg_pred
#' @param relative [\code{logical(1)}]\cr
#'   If \code{TRUE} rows are normalized to show relative frequencies.
#'   Default is \code{FALSE}.
#' @param sums {\code{logical(1)}}\cr
#'   If \code{TRUE} add absolut or relative number of observations in each group.
#' @return [\code{matrix}]. A confusion matrix.
#' @export
#' @seealso \code{\link{predict.WrappedModel}}
#' @examples
#' # get confusion matrix after simple manual prediction
#' allinds = 1:150
#' train = sample(allinds, 75)
#' test = setdiff(allinds, train)
#' mod = train("classif.lda", iris.task, subset = train)
#' pred = predict(mod, iris.task, subset = test)
#' print(getConfMatrix(pred))
#' print(getConfMatrix(pred, relative = TRUE))
#'
#' # now after cross-validation
#' r = crossval("classif.lda", iris.task, iters = 2L)
#' print(getConfMatrix(r$pred))

getConfMatrix = function(pred, relative = FALSE, sums = FALSE) {
  checkPrediction(pred, task.type = "classif", check.truth = TRUE, no.na = TRUE)
  assertFlag(relative)
  cls = getTaskClassLevels(pred$task.desc)
  k = length(cls)
  n = getTaskSize(pred$task.desc)
  resp = getPredictionResponse(pred)
  truth = getPredictionTruth(pred)
  tab = table(truth, resp)
  # create table for margins, where only the off-diag errs are in
  mt = tab; diag(mt) = 0
  row.err = rowSums(mt)
  col.err = colSums(mt)
  result = rbind(cbind(tab, row.err), c(col.err, sum(col.err)))
  dimnames(result) = list(true = c(cls, "-err.-"), predicted = c(cls, "-err.-"))
  js = 1:k # indexes for nonmargin cols

  if (relative) {
    rownorm = function(r) {
      if (any(r[js] > 0))
        r / sum(r[js])
      else
        rep(0, k + 1)
    }
    result[js, ] = t(apply(result[js, ], 1, rownorm))
    #colsums of offdiagonal elements
    result[k+1, js] = colSums(result[js, js]) - diag(result[js, js])
    result[k+1, k+1] = result[k+1, k+1] / n
  }

  if (sums) {
    rowsum = rowSums(tab)
    colsum = colSums(tab)
    result = rbind(cbind(result, c(rowsum, NA)), c(colsum, NA, n))
    colnames(result)[k + 2] = "-N-"
    rownames(result)[k + 2] = "-N-"
  }
  return(result)
}
