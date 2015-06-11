#' @title Confusion matrix.
#'
#' @description
#' Calculates confusion matrix for (possibly resampled) prediction.
#' Rows indicate true classes, columns predicted classes.
#'
#' @template arg_pred
#' @param relative [\code{logical(1)}]\cr
#'   If \code{TRUE} rows are normalized to show relative frequencies.
#'   Default is \code{FALSE}.
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
getConfMatrix = function(pred, relative = FALSE) {
  checkPrediction(pred, task.type = "classif", check.truth = TRUE, no.na = TRUE)
  assertFlag(relative)
  cls = pred$task.desc$class.levels
  k = length(cls)
  resp = pred$data$response
  truth = pred$data$truth
  tab = table(truth, resp)
  mt = tab * (matrix(1, ncol = k, nrow = k) - diag(1, k, k))
  rowsum = rowSums(mt)
  colsum = colSums(mt)
  result = rbind(cbind(tab, rowsum), c(colsum, sum(colsum)))
  dimnames(result) = list(true = c(cls, "-SUM-"), predicted = c(cls, "-SUM-"))
  if (relative) {
    total = sum(result[1:k, 1:k])
    k1 = k + 1
    result[k1, 1:k] = if (result[k1, k1] != 0)
      result[k1, 1:k] / result[k1, k1]
    else
      0
    rownorm = function(r, len) {
      if (any(r[1:len] > 0))
        r / sum(r[1:len])
      else
        rep(0, len + 1)
    }
    result[1:k, ] = t(apply(result[1:k, ], 1, rownorm, len = k))
    result[k1, k1] = result[k1, k1] / total
  }
  return(result)
}
