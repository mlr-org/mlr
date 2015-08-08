#' @title Feature-subspace Aggregating Ensemble algorithm
#' 
#' @description An ensemble algorithm which builds local models instead of global models. 
#' The algorithm builds several trees to split the data space then train a model on each leaf.
#' The final result is the aggregation of results from all the trees.
#' 
#' @param learner a classification learner object
#' @param attrs an integer vector indicating the indices of the features taken into 
#'   tree building algorithm.
#' @param random an integer indicating the number of trees. All combination of trees will be 
#'   considered if set to 0 (this might be very expensive).
#' @param maximum.level the maximum.level of the tree, the root is level 0.
#' @param minimum.instance the minimum number of data instances to train a learner on each leaf.
#'   The majority or proportion will be used as prediction otherwise.
#' @param predict.type either \code{response} or \code{prob} indicating the format of the prediction.
#' 
#' @examples
#' data(iris)
#' lrn = makeLearner("classif.svm")
#' tsk = makeClassifTask(data = iris, target = "Species")
#' flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:4, random = 5, maximum.level = 1,
#'   minimum.instance = 4, predict.type = "prob")
#' m = train(flrn, tsk)
#' pred = predict(m, newdata = iris)
#' 
#' @export
makeFeatingEnsemble = function(learner, attrs, random, maximum.level, minimum.instance, 
  predict.type = "response") {
  assertInteger(attrs, lower = 1)
  assertInt(random, lower = 0)
  assertInt(maximum.level, lower = 0, upper = length(attrs))
  assertInt(minimum.instance, lower = 1)
  assertChoice(predict.type, choices = c("response", "prob"))
  lrn = makeS3Obj(
    c("FeatingEnsemble", "Learner"),
    id = "feating",
    name = "Feature-subspace Aggregating Ensemble",
    short.name = "Feating",
    type = "classif",
    package = learner$package,
    fix.factors.prediction = learner$fix.factors.prediction, 
    properties = intersect(learner$properties,c("twoclass", "multiclass", "missings", "numerics", "factors", "prob", "weights")),
    predict.type = predict.type
  )
  learner = setPredictType(learner, predict.type)
  lrn$learner = learner
  lrn$attrs = attrs
  lrn$random = random
  lrn$maximum.level = maximum.level
  lrn$minimum.instance = minimum.instance
  return(lrn)
}

#' @export
trainLearner.FeatingEnsemble = function(.learner, .task, .subset, ...) {
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  target = getTaskTargetNames(.task)
  td = .task$task.desc
  
  forest = list()
  random = .learner$random
  attrs = .learner$attrs
  maximum.level = .learner$maximum.level
  minimum.instance = .learner$minimum.instance
  learner = .learner$learner
  
  n = length(attrs)
  if (n >= ncol(data)) 
    stop("Too many features in `attrs`.")
  target.ind = which(target == names(data))
  attr.order = combn(n, maximum.level+1)
  binary.ind = colSums(attr.order == target.ind)
  binary.ind = which(binary.ind == 0)
  attr.order = attr.order[,binary.ind]
  N = ncol(attr.order)

  if (random == 0) {
    for (i in 1:N) {
      attr.ind = attrs[attr.order[,i]]
      forest[[i]] = buildLevelTree(data = data, target = target, learner = learner,
        attr.ind = attr.ind, current.level = 0, maximum.level = maximum.level, 
        minimum.instance = minimum.instance, task.desc = td)
    }
  } else {
    assertInt(random, lower = 1, upper = N)
    ind = sample(1:N, random)
    attr.order = attr.order[,ind]
    for (i in 1:random) {
      attr.ind = attrs[attr.order[,i]]
      forest[[i]] = buildLevelTree(data = data, target = target, learner = learner,
        attr.ind = attr.ind, current.level = 0, maximum.level = maximum.level, 
        minimum.instance = minimum.instance, task.desc = td)
    }
  }
  return(forest)
}

#' @export
predictLearner.FeatingEnsemble = function(.learner, .model, .newdata, ...) {
  m = length(.model$learner.model)
  sm.pt = .model$learner$predict.type
  td = .model$task.desc
  levs = td$class.levels
  p = length(levs)
  n = nrow(.newdata)
  pred.mat = matrix(0, n, p)
  for (i in 1:m) {
    pred = predictLevelTree(.model$learner.model[[i]], .newdata, sm.pt)
    if (sm.pt == "response") {
      ind.mat = cbind(1:n, pred)
      pred.mat[ind.mat] = pred.mat[ind.mat]+1
    } else {
      pred.mat = pred.mat + pred
    }
  }
  if (sm.pt == "response") {
    prob.mat = rowiseRatio(pred.mat, levs)
  } else {
    prob.mat = pred.mat/m
  }
  if (sm.pt == "prob") {
    colnames(prob.mat) = levs
    return(prob.mat)
  } else {
    ind = max.col(prob.mat)
    pred = factor(levs[ind], levs)
    return(pred)
  }
}

buildLevelTree = function(data, target, learner, attr.ind, current.level = 0, maximum.level = 3,
  minimum.instance = 4, random.tree = FALSE, task.desc) {
  
  node = list()
  pt = learner$predict.type
  n = nrow(data)
  levs = task.desc$class.levels
  node$pred.dim = length(levs)
  node$levs = levs

  if (n <= minimum.instance) {
    # stb = sort(table(data[,target]), decreasing = TRUE)
    stb = sapply(levs, function(x) sum(as.character(data[,target]) == x))
    
    node$type = "minimum"
    if (pt == "response") {
      node$major = which.max(stb)
    } else {
      node$major = unname(stb)/sum(stb)
    }
    return(node)
  }
  
  if (current.level == maximum.level) {
    task = makeClassifTask(data = data, target = target)
    model = train(learner, task)
    node$type = "model"
    node$model = model
    return(node)
  }
  
  v = attr.ind[current.level+1]
  missing.ind = which(is.na(data[,v]))
  missing.branch = list()
  if (length(missing.ind)>0) {
    if (!hasLearnerProperties(learner, "missings"))
      stop("The learner doesn't support missing value prediction, but the data has missing values.")
    missing.branch = buildLevelTree(data[missing.ind, , drop = FALSE], target, 
      learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
      maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
    data = data[-ind, , drop = FALSE]
  }
  
  if (class(data[,v]) == "numeric") {
    m = 2
    split.point = findSplitPoint(data[,v], data[,target], random.tree = random.tree)
    ind1 = which(data[,v]<=split.point)
    ind2 = setdiff(1:n, ind1)
    node$type = "numeric"
    node$attr = v
    node$split.point = split.point
    
    node$branch = list()
    node$branch[[1]] = buildLevelTree(data[ind1, , drop = FALSE], target, 
      learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
      maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
    node$branch[[2]] = buildLevelTree(data[ind2, , drop = FALSE], target, 
      learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
      maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
  } else {
    cls = unique(data[,v])
    node$type = "factor"
    node$attr = v
    node$branch = list()
    node$cls = as.list(cls)
    if (!random.tree) {
      m = length(cls)
      for (i in 1:m) {
        ind = which(data[,v] == cls[i])
        node$branch[[i]] = buildLevelTree(data[ind, , drop = FALSE], target, 
          learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
          maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
      }
    } else {
      m = 2
      cls = sample(cls)
      p = length(cls) %/% 2
      ind1 = sapply(cls[1:p], function(x) which(x == data[,v]))
      ind2 = setdiff(1:n, ind1)
      node$cls = list(cls[1:p], cls[-(1:p)])
      
      node$branch[[1]] = buildLevelTree(data[ind1, , drop = FALSE], target, 
        learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
        maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
      node$branch[[2]] = buildLevelTree(data[ind2, , drop = FALSE], target, 
        learner = learner, attr.ind = attr.ind, current.level = current.level+1, 
        maximum.level = maximum.level, minimum.instance = minimum.instance, random.tree = random.tree, task.desc = task.desc)
    }
  }
  if (length(missing.ind)>0) {
    node$branch[[m+1]] = missing.branch
  }
  
  return(node)
}

predictLevelTree = function(node, newdata, predict.type) {
  n = nrow(newdata)
  if (predict.type == "response") {
    pred = matrix(0, n, 1)
  } else {
    pred = matrix(0, n, node$pred.dim)
  }
  if (node$type == "model") {
    pred = predict(node$model, newdata = newdata)
    resp = getPredictionResponse(pred)
    if (predict.type == "response") {
      pred = as.numeric(resp)
    } else {
      prob = getPredictionProbabilities(pred)
      if (node$pred.dim == 2) {
        pred = cbind(prob, 1-prob)
      } else {
        if (is.vector(prob)) {
          lvs = levels(resp)
          levs = node$levs
          pred = matrix(0, n, node$pred.dim)
          ind = match(lvs, levs)
          pred[,ind] = cbind(prob, 1-prob)
        } else {
          pred = data.matrix(prob)
        }
      }
    }
  } else if (node$type == "minimum") {
    if (predict.type == "response") {
      pred = rep(node$major, n)
    } else {
      pred = matrix(rep(node$major, n), ncol = n)
      pred = t(pred)
    }
  } else if (node$type == "factor") {
    # need training set judging if support missing values or not
    v = node$attr
    missing.ind = which(is.na(newdata[,v]))
    m = length(node$branch)
    cls = node$cls
    
    if (length(missing.ind)>0) {
      if (m == length(cls)) {
        stop("No missing values in training data, but in test data.")
      } else {
        pred[missing.ind,] = predictLevelTree(node$branch[[1]])
      }
    }
    for (i in 1:m) {
      ind = sapply(cls, function(x) which(newdata[,v]==x))
      pred[ind,] = predictLevelTree(node$branch[[i]], newdata[ind, , drop = FALSE], predict.type)
    }
  } else {
    v = node$attr
    missing.ind = which(is.na(newdata[,v]))
    if (length(missing.ind)>0){
      if (length(node$branch) == 2) {
        stop("No missing values in training data, but in test data.")
      } else {
        pred[missing.ind,] = predictLevelTree(node$branch[[1]], newdata[missing.ind, , drop = FALSE], predict.type)
      }
    }
    split.point = node$split.point
    ind1 = which(newdata[,v] <= split.point)
    ind2 = setdiff(1:n, ind1)
    pred[ind1,] = predictLevelTree(node$branch[[1]], newdata[ind1, , drop = FALSE], predict.type)
    pred[ind2,] = predictLevelTree(node$branch[[2]], newdata[ind2, , drop = FALSE], predict.type)
  }
  return(pred)
}

findSplitPoint = function(feature, target, random.tree = FALSE) {
  n = length(feature)
  if (random.tree) {
    ind = sample(1:n, 1)
    return(feature[ind])
  }
  ind = order(feature)
  feature = feature[ind]
  target = target[ind]
  proportion = (1:n)/n
  
  cls = unique(target)
  p = length(cls)
  prob.mat1 = matrix(0,n,p)
  prob.mat2 = matrix(0,n,p)
  for (i in 1:p) {
    tmp = target==cls[i]
    prob.mat1[,i] = cumsum(tmp)
    prob.mat2[,i] = cumsum(rev(tmp))
  }
  prob.mat1 = prob.mat1/n
  prob.mat2 = prob.mat2/n
  log.prob.mat1 = log(prob.mat1, 2)
  log.prob.mat2 = log(prob.mat2, 2)
  ent.vec1 = -rowSums(prob.mat1*log.prob.mat1, na.rm = TRUE)
  ent.vec2 = -rowSums(prob.mat2*log.prob.mat2, na.rm = TRUE)
  ent.vec2 = rev(ent.vec2)
  
  ent.vec = ent.vec1[-n] + ent.vec2[-1]
  ind = which.max(ent.vec)
  split.point = (feature[ind] + feature[ind+1])/2
  return(split.point)
}
