# models$model$meta contains 3 entry, the resolution level, the start and end position of the segment
#
additivePredict = function(m, betas, intercept, models, X) {
  n = nrow(X)
  if (m == 0) return(rep(intercept, n)) # if we had 0 models in the past, just return intercept, otherwise predict first m model, multiply with beta, add intercept f_0
  p = sapply(1:m, function(i) {
    newX = X[, models[[i]]$model$meta[2]:models[[i]]$model$meta[3]]
    newX = as.matrix(apply(newX, 1, getSegmentFeatures))
    df = data.frame(newX)
    colnames(df) = models[[i]]$model$dfcolname
    pred = predict(object = models[[i]]$model$mod, newdata = df)
    (as.matrix(pred$data))
  })
  # p = cbind(p, predict(models[[i]], newdata = X))
  p %*% betas[1:m] + intercept
}

# loss = 0.5 (yhat - y)^2
# negative gradient of loss, here for L2-loss
dloss = function(y, yhat) (y - yhat)

# get beta by line search
# (bonus points to reader who sees that this is actually unnessary and stupid for L2 loss...)
lineSearch = function(y, yhat, rhat) {
  # L2 loss for yhat + beta * f.m
  obj = function(beta) crossprod(y - (yhat + beta * rhat))
  # find best beta
  or = optimize(obj, interval = c(0, 10000))
  print(or)
  or$minimum
}

multiResBoost = function(X, y, M = 10, res.level = 3L, shift = 0.5) {
  # for L2 loss we take the mean as constant model f_0
  intercept = mean(y)
  # here store the M models and associated beta weights
  models = vector("list", M)
  betas = numeric(M)
  for (j in 1:M) {
    messagef("Iteration: %i", j)
    # get predictions of our additive model
    yhat = additivePredict(m = j - 1L, betas = betas, intercept = intercept, models = models, X = X)
    # now get pseudo-residuals / negative gradient
    r = dloss(y, yhat)
    # fit model to pseudo residuals
    model2pred = getAggBestModelObs(X = X, y = r, res.level = 3L,shift = 0.5)
    models[[j]] = model2pred
    rhat = model2pred$pred
    beta = 1
    #beta = lineSearch(y = y, yhat = yhat, rhat = rhat)
    betas[[j]] = beta
  }
  return(list(models = models, betas = betas, intercept = intercept, n_rounds = M))
}

getAggBestModelObs = function(X, y, res.level = 3L, shift = 0.5) {
  mcolname = "x"
  n.obs = length(y)
  feat.list = vector("list", n.obs)
  pos = getCurveFeatures(X[1, ], res.level = res.level, shift = shift)$pos
  for (i in 1:n.obs) {
    featvec = numeric(0L)
    f = getCurveFeatures(X[i, ], res.level = res.level, shift = shift)
    featvec = c(featvec, f$feats)
    feat.list[[i]] = featvec  # put features from the ith instance into the list ith position
  }
  newX = do.call(rbind, feat.list)  # create a matrix by combining the row
  ndata = cbind(newX, y)
  ndata = data.frame(ndata)
  colnames(ndata) = c(paste0(mcolname,as.character(c(1:ncol(newX)))),"target")
  task = makeRegrTask(data = ndata, target = "target")
  lrn = makeLearner("regr.randomForest")
  model = train(lrn, task)
  mod = getLearnerModel(model)
  imp = getFeatureImportance(model)
  res = sort(imp$res, decreasing = TRUE)
  j = as.numeric(which(names(res[1]) == names(imp$res)))
  meta = pos[[j]]
  # single variable classification
  df = data.frame(cbind(newX[, j], y))
  colnames(df) = c(mcolname, "target")
  task = makeRegrTask(data = df, target = "target")
  lrn = makeLearner("regr.rpart")
  model = train(lrn, task)
  #
  res.model = list(mod = model, meta = meta, dfcolname = mcolname)
  newdata = data.frame(newX[, j])
  colnames(newdata) = mcolname
  pred = predict(model, newdata = newdata)
  return(list(model = res.model, pred = pred))
}
