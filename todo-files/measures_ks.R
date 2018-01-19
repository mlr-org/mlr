#' @export ks
#' @rdname measures
#' @usage none
#' @format none
ks = makeMeasure(id = "ks", minimize = FALSE, classif = TRUE, only.binary = TRUE, allowed.pred.types = "prob",
  fun = function(task, model, pred, extra.args) {
    pos = pred$task.desc$positive
    idx1 = pred$data$truth == pos
    col1 = paste0("prob.", pos)
    x = pred$data[ idx1, col1]
    y = pred$data[!idx1, col1]
    n = length(x)
    w = c(x, y)
    z = cumsum(ifelse(order(w) <= n, 1/n, -1/n))
    if (length(unique(w)) < 2*n)
      z = z[c(which(diff(sort(w)) != 0), 2*n)]
    max(abs(z))
  }
)

