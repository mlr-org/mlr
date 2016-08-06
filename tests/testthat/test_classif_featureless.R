context("classif_featureless")

test_that("classif_featureless", {
  costs = matrix(c(0, 10, 10, 1, 0, 1, 1, 1, 0), 3, 3, byrow = TRUE)
  rownames(costs) = colnames(costs) = getTaskClassLevels(iris.task)
  print(costs)
  mymeas = makeCostMeasure(costs = costs)
  
  getBestPrediction = function(task, measure) {
    levs = getTaskClassLevels(task)
    y = getTaskTargets(task)
    n = length(y)
    scores = vnapply(levs, function(a) {
      arep = factor(rep(a, n), levels = levs)
      data = data.frame(truth = y, response = arep)
      desc = makeS3Obj("TaskDesc", class.levels = levs)
      p = makeS3Obj("Prediction", data = data, task.desc = desc)
      measure$fun(pred = p, extra.args = measure$extra.args)
    })
    names(scores)[which.min(scores)]
  }
  
  levs = getTaskClassLevels(iris.task)
  lev1 = getBestPrediction(iris.task, mmce)
  lev2 = getBestPrediction(iris.task, mymeas)
  p1 = factor(rep(lev1, 60), levels = levs)  #dim(multiclass.df)[1] = 150
  p2 = factor(rep(lev2, 60), levels = levs)  #dim(multiclass.df)[1] = 150
  
  
  testSimple(t.name = "classif.featureless", df = multiclass.df, target = multiclass.target, train.inds = multiclass.train.inds, old.predicts = p1, parset = list(measure = mmce))
  
  testSimple(t.name = "classif.featureless", df = multiclass.df, target = multiclass.target, train.inds = multiclass.train.inds, old.predicts = p2, parset = list(measure = mymeas))
  
})