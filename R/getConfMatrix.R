#' Confusion matrix.
#'
#' Calculates confusion matrix for (possibly resampled) prediction.
#' Rows indicate true classes, columns predicted classes.
#'
#' Code inspired by \code{\link[klaR]{errormatrix}}.
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
  assertClass(pred, classes = "Prediction")
  assertFlag(relative)

  if (pred$task.desc$type != "classif")
    stop("Can only calculate confusion matrix for classification predictions, not: %s",
      pred$task.desc$type)
  resp = pred$data$response
  cls = pred$task.desc$class.levels
  n = length(cls)
  tab = table(pred$data$truth, resp)
  mt = tab * (matrix(1, ncol = n, nrow = n) - diag(, n, n))
  rowsum = rowSums(mt)
  colsum = colSums(mt)
  result = rbind(cbind(tab, rowsum), c(colsum, sum(colsum)))
  dimnames(result) = list(true = c(cls, "-SUM-"),
                          predicted = c(cls, "-SUM-"))
  if (relative) {
    # FIXME: this is quite inefficient
    total = sum(result[1:n, 1:n])
    n1 = n + 1
    result[n1, 1:n] = if (result[n1, n1] != 0) result[n1, 1:n]/result[n1, n1] else 0
    rownorm = function(Row, Length) {
      return(if (any(Row[1:Length] > 0)) Row/sum(Row[1:Length])
             else rep(0, Length + 1))
    }
    result[1:n, ] = t(apply(result[1:n, ], 1, rownorm, Length = n))
    result[n1, n1] = result[n1, n1]/total
  }
  return(result)
}
