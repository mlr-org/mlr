# mainly copied from the caTools package
colAUC = function(probabilities, truth) {
  y = as.factor(truth)
  X = as.matrix(probabilities)
  if (nrow(X) == 1)
    X = t(X)
  nR = nrow(X)
  nC = ncol(X)
  nY = table(y)
  uL = as.factor(rownames(nY))
  nL = length(nY)
  if (nL <= 1) 
    stop("colAUC: List of labels 'y' have to contain at least 2 class labels.")
  if (!is.numeric(X)) 
    stop("colAUC: 'X' must be numeric")
  if (nR != length(y)) 
    stop("colAUC: length(y) and nrow(X) must be the same")
  L = matrix(rep(uL, each = nR), nR, nL)
  per = combs(1:nL, 2)
  nP = nrow(per)
  Auc = matrix(0.5, nP, nC)
  rownames(Auc) = paste(uL[per[, 1]], " vs. ", uL[per[, 2]], sep = "")
  colnames(Auc) = colnames(X)
  # Wilcoxon AUC
  idxL = vector(mode = "list", length = nL)
  for (i in 1:nL) idxL[[i]] = which(y == uL[i])
  for (j in 1:nC) {
    for (i in 1:nP) {
      c1 = per[i, 1]
      c2 = per[i, 2]
      n1 = as.numeric(nY[c1])
      n2 = as.numeric(nY[c2])
      if (n1 > 0 & n2 > 0) {
        r = rank(c(X[idxL[[c1]], j], X[idxL[[c2]], j]))
        Auc[i, j] = (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / (n1 * n2)
      }
    }
  }
  Auc = pmax(Auc, 1 - Auc)
  return(Auc)
}

combs = function(v, k) {
  # combs(V,K) - finds all unordered combinations of K elements from vector V 
  #  V is a vector of length N
  #  K is a integer 
  # combs(V,K) creates a matrix with N!/((N-K)! K!) rows
  # and K columns containing all possible combinations of N elements taken K at a time.
  # example: combs(1:3,2) returns matrix with following rows (1 2), (1 3), (2 3)
  n = length(v)
  if (n == k) P = matrix(v, 1, n)
  else if (k == 1) P = matrix(v, n, 1)
  else if (k == n-1) P = matrix(rep(v, each = n - 1), n, n - 1)
  else if (k < n) {
    P = matrix(0, 0, k)
    if (k < n & k > 1) {
      for (i in 1:(n - k + 1)) {
        Q = combs(v[(i + 1):n], k - 1)
        j = nrow(Q)
        P = rbind(P, cbind(rep(v[i], j), Q))
      }
    }
  } else 
    stop("combs: number m has to be smaller or equal to length of vector v")
  return(P)
}