context("regr_cv.glmnet")

# test_that("regr_cv.glmnet", {
#   library(glmnet)
#   parset.list = list(
#     list(),
#     list(alpha = 0.7),
#     list(s = 0.3)
#   )
#
#   old.predicts.list = list()
#
#   for (i in 1:length(parset.list)) {
#     parset = parset.list[[i]]
#     s = parset[["s"]]
#     if(is.null(s)) s = 0.01
#     parset[["s"]] = NULL
#     ind = match(regr.target, names(regr.train))
#     x = regr.train[, -ind]
#     x$chas = as.numeric(x$chas)
#     y = regr.train[, ind]
#     pars = list(x = as.matrix(x), y = y, family = "gaussian")
#     pars = c(pars, parset)
#     ctrl.args = names(formals(glmnet.control))
#     if (any(names(pars) %in% ctrl.args)) {
#       do.call(glmnet.control, pars[names(pars) %in% ctrl.args])
#       m = do.call(cv.glmnet, pars[!names(pars) %in% ctrl.args])
#       glmnet.control(factory = TRUE)
#     } else {
#       m = do.call(cv.glmnet, pars)
#     }
#     newx = regr.test[,-ind]
#     newx$chas = as.numeric(newx$chas)
#     old.predicts.list[[i]] = predict(m, as.matrix(newx), s = s)[,1]
#   }
#   test.dat = regr.df
#   test.dat$chas = as.numeric(test.dat$chas)
#   testSimpleParsets("regr.cv.glmnet", test.dat, regr.target, regr.train.inds, old.predicts.list, parset.list)
# })
