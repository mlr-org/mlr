context("getOutOfBag")
test_that("getOutOfBag", {
  
  checkOutOfBag = function(lrn.id) {
    par.vals = list()
    lrn.id.split = unlist(strsplit(lrn.id, split = ".", fixed = TRUE))
    type = lrn.id.split[1L]
    alg = lrn.id.split[2L]
    if (type == "classif") {
      tsk = binaryclass.task
    } else {
      if (type == "regr") {
        tsk = regr.task
      } else {
        if (type == "surv") {
          tsk = surv.task
        } else {
          stop("should not happen")
        }
      } 
    }
    
    # you may want to change the params for certain learner if training takes
    # a long time
    if (alg == "randomForestSRC")
      par.vals$ntree = 50L
    
    lrn = makeLearner(lrn.id, par.vals = par.vals)
    mod = train(lrn, tsk)
    oob = getOutOfBag(mod, tsk)
    
    if (type == "classif") {
      expect_is(oob$data, "data.frame")
      expect_equal(levels(oob$data$response), tsk$task.desc$class.levels)
    } else {
      if (type %in% c("regr", "surv")) {
        expect_is(oob$data$response, "numeric")
      } else {
        stop("should not happen")
      }
    }
    expect_equal(nrow(oob$data), nrow(getTaskData(tsk)))
  }
  
  oob.methods = ls(getNamespace("mlr"), all.names = TRUE,
                   pattern = "getOutOfBag\\.", sorted = FALSE)
  lrn.ids = gsub("getOutOfBag.", "", oob.methods, fixed = TRUE)
  sapply(lrn.ids, checkOutOfBag)
})