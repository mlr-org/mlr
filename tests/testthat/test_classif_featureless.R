context("classif_featureless")

test_that("classif_featureless", {
  costs = matrix(c(0, 10, 10, 1, 0, 1, 1, 1, 0), 3, 3, byrow = TRUE)
  rownames(costs) = colnames(costs) = getTaskClassLevels(iris.task)
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
    if (measure$minimize)
      factor(names(scores)[which.min(scores)], levels = levs)
    else
      factor(names(scores)[which.max(scores)], levels = levs)
  }
  
  levs = getTaskClassLevels(iris.task)
  lev1 = getBestPrediction(iris.task, mmce)
  lev2 = getBestPrediction(iris.task, mymeas)
  p1 = factor(rep(lev1, 60), levels = levs)  #dim(multiclass.df)[1] = 150
  p2 = factor(rep(lev2, 60), levels = levs)  #dim(multiclass.df)[1] = 150
  
  testSimple(t.name = "classif.featureless", df = multiclass.df, target = multiclass.target, train.inds = multiclass.train.inds, old.predicts = p1, parset = list(measure = mmce))
  testSimple(t.name = "classif.featureless", df = multiclass.df, target = multiclass.target, train.inds = multiclass.train.inds, old.predicts = p2, parset = list(measure = mymeas))
  
  # test that bad measures cannot be used
  expect_error(train(makeLearner("classif.featureless", measure = auc), iris.task),
    "Multiclass problems cannot be used for measure auc!")
  expect_error(train(makeLearner("classif.featureless", measure = timetrain), binaryclass.task),
    "requires a fitted model")
  expect_error(train(makeLearner("classif.featureless", measure = mse), binaryclass.task),
    "Measure mse does not support task type classif!")
  
  # test that printers work correctly and print the measure id and not <measure>
  lrn = makeLearner("classif.featureless", measure = auc)
  expect_output(print(lrn), "auc")
  expect_output(print(getHyperPars(lrn)), "auc")
})