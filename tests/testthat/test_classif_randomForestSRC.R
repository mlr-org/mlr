
test_that("classif_randomForestSRC", {
  # skip_on_os("mac")
  requirePackagesOrSkip("randomForestSRC", default.method = "load")


  library(mlr)

  data(Sonar, package = "mlbench", envir = environment())

  binaryclass.df = Sonar
  binaryclass.formula = Class ~ .
  binaryclass.target = "Class"
  binaryclass.train.inds = c(1:50, 100:150)
  binaryclass.test.inds = setdiff(seq_len(nrow(binaryclass.df)), binaryclass.train.inds)
  binaryclass.train = binaryclass.df[binaryclass.train.inds, ]
  binaryclass.test = binaryclass.df[binaryclass.test.inds, ]
  binaryclass.class.col = 61
  binaryclass.class.levs = levels(binaryclass.df[, binaryclass.class.col])
  binaryclass.task = makeClassifTask("binary", data = binaryclass.df, target = binaryclass.target)

  parset.list = list(
    # list(ntree = 200),
    list(ntree = 350, mtry = 5L, na.action = "na.impute")#,
    # list(ntree = 250, nodesize = 2, na.action = "na.impute",
      # importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train,
      formula = binaryclass.formula, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    lrn = do.call("makeLearner", c(list(cl = "classif.randomForestSRC"), list(ntree = 350, mtry = 5L, na.action = "na.impute")))
    task = makeClassifTask(data = binaryclass.df, target =  binaryclass.target)
    m_mlr = train(lrn, task, subset = binaryclass.train.inds)

    # direct
    p = predict(m, newdata = binaryclass.test[, 1:60], membership = FALSE,
      na.action = "na.impute")
    # direct with mlr learner.model
    p_mlr = predict(m_mlr$learner.model, newdata = binaryclass.test[, 1:60], membership = FALSE,
                    na.action = "na.impute")
    # mlr predict
    p_mlr_pred = predict(m_mlr, newdata = binaryclass.test[, 1:60])

    p$predicted[71]
    p_mlr$predicted[71]
    # prob is not returned from mlr here

  }

  p$class[71]
  p_mlr$class[71]
  p_mlr_pred$data$response[71] # problem

  waldo::compare(p$class[71], p_mlr$class[71])
  waldo::compare(p_mlr$class[71], p_mlr_pred$data$response[71])



  # testSimpleParsets("classif.randomForestSRC", binaryclass.df,
  #   binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  # testProbParsets("classif.randomForestSRC", binaryclass.df,
  #   binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
