additivePredict = function(m, n, betas, intercept, models, X) {
  n = nrow(X)
  if (m == 0) return(rep(intercept, n)) # if we had 0 models in the past, just return intercept, otherwise predict first m model, multiply with beta, add intercept f_0
  p = sapply(1:m, function(i) { predict(models[[i]], newdata = X) })
  # p = cbind(p, predict(models[[i]], newdata = X))
  p %*% betas[1:m] + intercept
}

# loss = 0.5 (yhat - y)^2
# negative gradient of loss, here for L2-loss
dloss = function(yhat) (y - yhat)

# get beta by line search
# (bonus points to reader who sees that this is actually unnessary and stupid for L2 loss...)
lineSearch = function(yhat, rhat) {
  # L2 loss for yhat + beta * f.m
  obj = function(beta) crossprod(y - (yhat + beta * rhat))
  # find best beta
  or = optimize(obj, interval = c(0, 10000))
  print(or)
  or$minimum
}

multiResBoost = function(X, y, M = 10, curve.lens, res.level = 3L, shift = 0.5) {
  n = length(y)
  # for L2 loss we take the mean as constant model f_0
  intercept = mean(y)
  # here store the M models and associated beta weights
  models = vector("list", M)
  betas = numeric(M)
  for (j in 1:M) {
    messagef("Iteration: %i", j)
    # get predictions of our additive model
    yhat = additivePredict(m = j - 1L, n = n, betas = betas, intercept = intercept, models = models, X = X)
    # now get pseudo-residuals / negative gradient
    r = dloss(yhat)
    # fit model to pseudo residuals
    model2pred = getAggBestModelObs(X = X, y = r, res.level = 3L,shift = 0.5)
    models[[j]] = model2pred$model
    rhat = model2pred$pred
    beta = lineSearch(yhat = yhat, rhat = rhat)
    betas[[j]] = beta
  }
  return(list(models = models, betas = betas, intercept = intercept))
}

getAggBestModelObs = function(X, y, res.level = 3L,shift = 0.5) {
  n.obs = length(y)
  feat.list = vector("list", n.obs)
  for (i in 1:n.obs) {
    featvec = numeric(0L)
    f = getCurveFeatures(X[i, ], res.level = res.level, shift = shift)
    featvec = c(featvec, f)
    feat.list[[i]] = featvec  # put features from the ith instance into the list ith position
  }
  do.call(rbind, feat.list)  # create a matrix by combining the row
  return(list(model = model, pred = pred))
}
