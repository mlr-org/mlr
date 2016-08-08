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
  
  if (sums) {
    rowsum = rowSums(tab)
    colsum = colSums(tab)
    result = rbind(cbind(result, c(rowsum, NA)), c(colsum, NA, n))
    colnames(result)[k + 2] = "-n-"
    rownames(result)[k + 2] = "-n-"
  }
  
  js = 1:k # indexes for nonmargin cols

  if (relative) {
    
    norm.conf.matrix = function(r) {
      if (any(r[js] > 0))
        r / sum(r[js])
      else
        rep(0, k + 1)
    }
    
    #normalize by rows and add margins as a new column
    result.rel.row = t(apply(tab, 1, norm.conf.matrix))
    result.rel.row = cbind(result.rel.row, "-err-" = rowSums(result.rel.row) - diag(result.rel.row))
    
    #normalize by columns and add margins as a new row
    result.rel.col = apply(tab, 2, norm.conf.matrix)
    result.rel.col = rbind(result.rel.col, "-err-" = colSums(result.rel.col) - diag(result.rel.col))
    
    result = list(absolute = result, relative.row = result.rel.row, relative.col = result.rel.col, k = k, n = n)
  }

  addClasses(result, "confMatrix")
}

print.confMatrix = function(cm, digits = 2, nsmall = 2, ...) {
  if(!is.list(cm)) {
    print(cm)
  }
  else {
    js = 1:cm$k
    res = paste(format(cm$relative.row[js, js], digits = digits, nsmall = nsmall, ...), 
      format(cm$relative.col[js, js], digits = digits, nsmall = nsmall, ...), sep = "/")
    attributes(res) = attributes(cm$relative.row[js, js])
    result = rbind(cbind(tab, cm$relative.row), c(col.err, sum(col.err)))
    noquote(res)
  }
}
