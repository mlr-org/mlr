context("learners_classiflabelswitch")

n = 50L
p = 2L
mydata1 = matrix(runif(2 * n * p), nrow = 2 * n, ncol = p)
mydata1 = as.data.frame(mydata1)
mydata1[1:n, ] = mydata1[1:n, ] + 10L
mydata1[(n + 1):(2 * n), ] = mydata1[(n + 1):(2 * n), ] - 10L
mydata1$y = factor(rep(c("a", "b"), each = c(n)))
mydata2 = mydata1
mydata2$y = factor(rep(c("a", "b"), each = c(n)), levels = c("b", "a"))

mydata3 = matrix(runif(3 * n * p), nrow = 3 * n, ncol = p)
mydata3 = as.data.frame(mydata3)
mydata3[1:n, ] = mydata3[1:n, ] + 10L
mydata3[(n + 1):(2 * n), ] = mydata3[(n + 1):(2 * n), ] - 10L
mydata3$y = factor(rep(c("a", "b", "c"), each = c(n)))
mydata4 = mydata3
mydata4$y = factor(rep(c("a", "b", "c"), each = c(n)), levels = c("c", "b", "a"))

mytask1a = makeClassifTask(id = "t1a", data = mydata1, target = "y",
  positive = "a")
mytask1b = makeClassifTask(id = "t1b", data = mydata1, target = "y",
  positive = "b")
mytask2a = makeClassifTask(id = "t2a", data = mydata2, target = "y",
  positive = "a")
mytask2b = makeClassifTask(id = "t2b", data = mydata2, target = "y",
  positive = "b")
mytask3 = makeClassifTask(id = "t3", data = mydata3, target = "y")
mytask4 = makeClassifTask(id = "t4", data = mydata4, target = "y")

hpars = list(
  classif.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
    num_iterations_after_burn_in = 10L),
  classif.bdk = list(ydim = 2L),
  classif.boosting = list(mfinal = 10L),
  classif.cforest = list(mtry = 2L),
  classif.dbnDNN = list(numepochs = 10),
  classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
  classif.lssvm = list(kernel = "rbfdot", sigma = 0.4, reduced = FALSE),
  classif.LiblineaRLogReg = list(type = 7),
  classif.LiblineaRBinary = list(type = 1),
  classif.LiblineaRMultiClass = list(type = 1),
  classif.nodeHarvest = list(nodes = 100L, maxinter = 1L),
  classif.FDboost = list(mstop = 10L)
)


test_that("no labels are switched", {
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)


  checkErrsForTask = function(task, predtype) {

    props = if (predtype == "response") character(0L) else "prob"
    lrns = listLearners(task, create = TRUE, properties = props)
    lids = extractSubList(lrns, "id")
    names(lrns) = lids
    toremove = grepl("classif.mock", lids)
    toremove = toremove | grepl("classif.LiblineaRMultiClass", lids)
    toremove = toremove | grepl("classif.h2o", lids)
    toremove = toremove | grepl("classif.featureless", lids)
    lrns = lrns[!toremove]

    vnapply(lrns, function(lrn) {

      lrn = setPredictType(lrn, predtype)
      id = lrn$id
      hps = hpars[[id]]
      if (!is.null(hps)) {
        lrn = setHyperPars(lrn, par.vals = hps)
      }
      tmp = holdout(lrn, task, split = 0.5, stratify = TRUE)
      # print(as.data.frame(getRRPredictions(tmp)))
      err = tmp$aggr[[1L]]
      expect_true(!is.na(err) & err <= 1 / 3,
        info = paste(getTaskDesc(task)$id, id, err, sep = ", "))
      err
    })
  }
  # FIXME: only check prob for now for timimg reasons
  for (predtype in "prob") {
    checkErrsForTask(mytask1a, predtype)
    checkErrsForTask(mytask1b, predtype)
    checkErrsForTask(mytask2a, predtype)
    checkErrsForTask(mytask2b, predtype)
    checkErrsForTask(mytask3, predtype)
    checkErrsForTask(mytask4, predtype)
  }
})
