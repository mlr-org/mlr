# Be carefull that one need to change the columns name again after cbine several matrix
context("FDA_regr_pfr")
test_that("FDA_regr_prf", {
requirePackagesOrSkip("refund")  
data(DTI)
DTI1 <- DTI[DTI$visit==1 & complete.cases(DTI),]
# Fit model with additive functional term for CCA, using tensor product basis
fit.af <- pfr(pasat ~ af(cca, Qtransform=TRUE, k=c(7,7)), data=DTI1)

trafoListMat2df = function(list4mat, target, covariates){
  mdata = as.data.frame(Reduce(cbind, list(DTI1$cca, DTI1$pasat)))
  colnames(mdata)[length(colnames(mdata))] = target
  channel.list = list(CCA = 1:dim(DTI1$cca)[2] )
  return(list(mdata = mdata, target = target, channel.list = channel.list ))  
}

mu = trafoListMat2df(list4mat = DTI1, target = "pasat", covariates = c("cca"))
makeFDARegrTask(data = mu$mdata, target = m$target, channel.list = m$channel.list)
})