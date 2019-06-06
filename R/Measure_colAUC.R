# colAUC calculates for a vector with true values the Area Under the ROC Curve (AUC) for a matrix of samples.
# Matrix rows contain samples while the columns contain features/variables.
# The function is used to calculate different multiclass AUC measures AU1P, AU1U, AUNP, AUNU,
# following the definition by Ferri et al.:
# https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf

colAUC = function(samples, truth, maximum = TRUE) {

  y = as.factor(truth)
  X = as.matrix(samples)
  if (nrow(X) == 1) {
    X = t(X)
  }
  nr = nrow(X)
  nc = ncol(X)
  ny = table(y)
  ul = as.factor(rownames(ny))
  nl = length(ny)
  if (nl <= 1) {
    stop("colAUC: List of labels 'y' have to contain at least 2 class labels.")
  }
  if (!is.numeric(X)) {
    stop("colAUC: 'X' must be numeric")
  }
  if (nr != length(y)) {
    stop("colAUC: length(y) and nrow(X) must be the same")
  }
  per = t(utils::combn(1:nl, 2))
  np = nrow(per)
  auc = matrix(0.5, np, nc)
  rownames(auc) = paste(ul[per[, 1]], " vs. ", ul[per[, 2]], sep = "")
  colnames(auc) = colnames(X)
  # Wilcoxon AUC
  idxl = vector(mode = "list", length = nl)
  for (i in 1:nl) idxl[[i]] = which(y == ul[i])
  for (j in 1:nc) {
    for (i in 1:np) {
      c1 = per[i, 1]
      c2 = per[i, 2]
      n1 = as.numeric(ny[c1])
      n2 = as.numeric(ny[c2])
      if (n1 > 0 & n2 > 0) {
        r = rank(c(X[idxl[[c1]], j], X[idxl[[c2]], j]))
        auc[i, j] = (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / (n1 * n2)
      }
    }
  }
  if (maximum == TRUE) {
    auc = pmax(auc, 1 - auc)
  }
  return(auc)
}
