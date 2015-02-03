
n = 50L
p = 2L
mydata1 = matrix(runif(2*n*p), nrow = 2*n, ncol = p)
mydata1 = as.data.frame(mydata1)
mydata1[1:n,]  = mydata1[1:n,] + 10L
mydata1[(n+1):(2*n),]  = mydata1[(n+1):(2*n),] - 10L
mydata1$y = factor(rep(c("a", "b"), each = c(n)))
mydata2 = mydata1
mydata2$y = factor(rep(c("a", "b"), each = c(n)), levels = c("b", "a"))

mydata3 = matrix(runif(3*n*p), nrow = 3*n, ncol = p)
mydata3 = as.data.frame(mydata3)
mydata3[1:n,]  = mydata3[1:n,] + 10L
mydata3[(n+1):(2*n),]  = mydata3[(n+1):(2*n),] - 10L
mydata3$y = factor(rep(c("a", "b", "c"), each = c(n)))
mydata4 = mydata3
mydata4$y = factor(rep(c("a", "b", "c"), each = c(n)), levels = c("c", "b", "a"))

mytask1a = makeClassifTask(id = "t1a" , data = mydata1, target = "y", positive = "a")
mytask1b = makeClassifTask(id = "t1b", data = mydata1, target = "y", positive = "b")
mytask2a = makeClassifTask(id = "t2a", data = mydata2, target = "y", positive = "a")
mytask2b = makeClassifTask(id = "t2b", data = mydata2, target = "y", positive = "b")
mytask3 =  makeClassifTask(id = "t3",  data = mydata3, target = "y")
mytask4 =  makeClassifTask(id = "t4",  data = mydata4, target = "y")

hpars = list(
  classif.lssvm = list(kernel = "rbfdot", sigma = 0.4),
  classif.LiblineaRLogReg = list(type = 7),
  classif.LiblineaRBinary = list(type = 1),
  classif.LiblineaRMultiClass = list(type = 1)
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
    lrns = lrns[!toremove]

    errs = vnapply(lrns, function(lrn) {
      lrn = setPredictType(lrn, predtype)
      id = lrn$id
      hps = hpars[[id]]
      if (!is.null(hps))
        lrn = setHyperPars(lrn, par.vals = hps)
      holdout(lrn, task, split = 0.5, stratify = TRUE)$aggr[[1L]]
    })
    expect_true(all(!is.na(errs) & errs <= 0.3))
    # messagef("predtype = %s; task = %s", predtype, task$task.desc$id)
    # print(sort(errs, na.last = TRUE))
  }
  for (predtype in c("response", "prob")) {
    # checkErrsForTask(mytask1a, predtype)
    # checkErrsForTask(mytask1b, predtype)
    # checkErrsForTask(mytask2a, predtype)
    # checkErrsForTask(mytask2b, predtype)
    # checkErrsForTask(mytask3, predtype)
    # checkErrsForTask(mytask4, predtype)
  }
})

