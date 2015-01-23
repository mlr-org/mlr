
# n = 50L
# p = 2L
# mydata1 = matrix(runif(2*n*p), nrow = 2*n, ncol = p)
# mydata1 = as.data.frame(mydata1)
# mydata1[1:n,]  = mydata1[1:n,] + 10L
# mydata1[51:100,]  = mydata1[51:100,] - 10L
# mydata1$y = factor(rep(c("a", "b"), each = c(n)))

# hpars = list(
#   classif.lssvm = list(kernel = "vanilladot")
#   # classif.LiblineaRLogReg = list(type = 7),
#   # classif.LiblineaRBinary = list(type = 1),
#   # classif.LiblineaRMultiClass = list(type = 1)
# )


# test_that("two class works with predtype response", {
#   configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
#   mytask1 = makeClassifTask(data = mydata1, target = "y", positive = "a")
#   lrns = listLearners(mytask1, create = TRUE)
#   lids = extractSubList(lrns, "id")
#   names(lrns) = lids
#   toremove = grepl("classif.mock", lids)
#   toremove = toremove | grepl("classif.LiblineaRMultiClass", lids)
#   lrns = lrns[!toremove]
#   errs = vnapply(lrns, function(lrn) {
#     id = lrn$id
#     hps = hpars[[id]]
#     if (!is.null(hps))
#       lrn = setHyperPars(lrn, par.vals = hps)
#     holdout(lrn, mytask1, split = 0.5, stratify = TRUE)$aggr[[1L]]
#   })
#   print(lids)
#   print(sort(errs, na.last = TRUE))

# })

