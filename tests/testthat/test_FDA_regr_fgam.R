# Be carefull that one need to change the columns name again after cbine several matrix
context("FDA_regr_fgam")
test_that("FDA_regr_fgam", {
  # requirePackagesOrSkip("refund")
  # data(DTI)
  # DTI1 = DTI[DTI$visit == 1 & complete.cases(DTI),]
  # # Fit model with additive functional term for CCA, using tensor product basis
  # #fit.af = refund::pfr(formula = pasat ~ af(cca, Qtransform=TRUE, k=c(7,7)), data = DTI1)
  # #predict(fit.af, newdata = DTI1, type = 'response')
  # #########################################################################
  # #FIXME: the current implementation is not gneric
  # trafoListMat2df = function(list4mat, target, covariates){
  #   mdata = as.data.frame(Reduce(cbind, list(DTI1$cca, DTI1$pasat)))
  #   colnames(mdata)[length(colnames(mdata))] = target
  #   channel.list = list(cca = 1:dim(DTI1$cca)[2] )
  #   return(list(mdata = mdata, target = target, channel.list = channel.list ))
  # }
  # lrn = makeLearner("fdaregr.fgam", mgcv.s.k = -1L )
  # mu = trafoListMat2df(list4mat = DTI1, target = "pasat", covariates = c("cca"))
  # task = makeFDARegrTask(data = mu$mdata, target = mu$target, fd.features =  mu$channel.list)
  # mod1f = train(learner = lrn, task = task)
  # predict(object = mod1f, newdata = mu$mdata) # input data frame
})
