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
#' displays the total amount of errors. For the relative confusion matrix we normalize based on rows
#' and columns and create two seperate matricies.
#'
#' Note that for resampling no further aggregation is currently performed.
#' All predictions on all test sets are joined to a vector yhat, as are all labels
#' joined to a vector y. Then yhat is simply tabulated vs y, as if both were computed on
#' a single test set. This probably mainly makes sense when cross-validation is used for resampling.
#'
#' @template arg_pred
#' @param relative [\code{logical(1)}]\cr
#'   If \code{TRUE} two additional matricies are calculated. One is normalized by rows and one by
#'   columns, but we print the result in a compact way.
#' @param sums {\code{logical(1)}}\cr
#'   If \code{TRUE} add absolute number of observations in each group are added to the confusion matrix
#'   of absolute values.
#' @return [\code{confMatrix}]. A confusion matrix.
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
#' print(getConfMatrix(pred, sums = TRUE))
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
  
  result = list(result = result, k = k, n = n, cls = cls, relative = FALSE)
  
  js = 1:k # indexes for nonmargin cols

  if (relative) {
    
    norm.conf.matrix = function(r) {
      if (any(r[js] > 0))
        r / sum(r[js])
      else
        rep(0, k)
    }
    
    #normalize by rows and add margins as a new column
    result.rel.row = t(apply(tab, 1, norm.conf.matrix))
    result.rel.row = cbind(result.rel.row, "-err-" = rowSums(result.rel.row) - diag(result.rel.row))
    
    #normalize by columns and add margins as a new row
    result.rel.col = apply(tab, 2, norm.conf.matrix)
    result.rel.col = rbind(result.rel.col, "-err-" = colSums(result.rel.col) - diag(result.rel.col))
    
    result$relative.row = result.rel.row
    result$relative.col = result.rel.col
    result$relative = TRUE
  }

  addClasses(result, "confMatrix")
}

#' @export
#' @describeIn getConfMatrix
#' 
#' @param  x [\code{confMatrix}]\cr
#'  Result of \code{getConfMatrix}.
#' @param both [\code{logical(1)}]\cr
#'  If \code{TRUE} both the absolute and relative confusion matricies are printed.
#' @param digits [\code{numeric(1)}]\cr
#'  How many numbers after the decimal point should be printed, only relevant for relative confusion matricies.
#' @param ... \cr
#'  Currently not used.
print.confMatrix = function(x, both = TRUE, digits = 4, ...) {
  
  #formatting stuff, use digits numbers after(!) the decimal point.
  nsmall = digits
  digits = nsmall - 1
  
  if (x$relative) {
    js = 1:x$k
    res = paste(format(x$relative.row[js, js], digits = digits, nsmall = nsmall), 
      format(x$relative.col[js, js], digits = digits, nsmall = nsmall), sep = "/")
    attributes(res) = attributes(x$relative.row[js, js])
    
    
    col.err = x$relative.col[x$k + 1,]
    row.err = x$relative.row[,x$k + 1]
    full.err = paste(format(sum(row.err), digits = digits, nsmall = nsmall),
      format(sum(col.err), digits = digits, nsmall = nsmall), sep = "/")
    
    #bind marginal errors correctly formatted to rows and columns
    res = rbind(res, stri_pad_left(format(col.err, digits = digits, nsmall = nsmall), 
      width = nchar(full.err)))
    res = cbind(res, c(format(row.err, digits = digits, nsmall = nsmall), full.err))
    
    dimnames(res) = list(true = c(x$cls, "-err.-"), predicted = c(x$cls, "-err.-"))
    
    cat("Relative confusion matrix (normalized by row/column):\n")
    print(noquote(res))
    if (both) {
      cat("\n\nAbsolute confusion matrix:\n")
      print(x$result)
    }
  }
  else {
    print(x$result)
  }
}
