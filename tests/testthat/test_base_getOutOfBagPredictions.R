context("getOutOfBagPredictions")
test_that("getOutOfBagPredictions", {
  # function for checking all the learners, that support out of bag predictions
  checkOutOfBag = function(lrn.id) {
    lrn = makeLearner(lrn.id)
    if("randomForestSRC" %in% lrn$package)  # change defaults, so it is faster
      lrn = makeLearner(lrn.id, par.vals = list(ntree = 50L))
    type = lrn$type
    
    tsk = switch(type, 
      classif = binaryclass.task,
      regr = regr.task,
      surv = surv.task)
    mod = train(lrn, tsk)
    oob = getOutOfBagPredictions(mod, tsk)
    
    if (type == "classif") {
      expect_is(oob$data, "data.frame")
      expect_equal(levels(oob$data$response), tsk$task.desc$class.levels)
    } else {
      if (type %in% c("regr", "surv")) {
        expect_is(oob$data$response, "numeric")
      } 
    }
    expect_equal(nrow(oob$data), nrow(getTaskData(tsk)))
  }
  
  oob.methods = ls(getNamespace("mlr"), all.names = TRUE,
    pattern = "getOutOfBagPredictions\\.", sorted = FALSE)
  lrn.ids = gsub("getOutOfBagPredictions.", "", oob.methods, fixed = TRUE)
  sapply(lrn.ids, checkOutOfBag)
}) 
