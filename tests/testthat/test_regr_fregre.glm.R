test_that("test", {
  # create data-------------------------------------------------------------------
  requirePackagesOrSkip("fda.usc", default.method = "load")
  requirePackagesOrSkip("fda", default.method = "load")

  data("tecator", package = "fda.usc")
  leng = length(tecator$absorp.fdata$data[, 1])

  obs = as.numeric(c(rep(0, leng / 2), rep(1, leng / 2 + 1)))


  dat = cbind(obs, as.data.frame(tecator$absorp.fdata$data))
  colnames(dat) <- make.names(colnames(dat), unique = TRUE)

  fd.features = list("absorbtion" = 2:101)
  fdf = makeFunctionalData(dat, fd.features = fd.features)

  # perform learner---------------------------------------------------------------
  tsk1 = makeRegrTask("task1", data = fdf, target = "obs")

  fdalrn = makeLearner("regr.fregre.glm")


  basis.x = create.bspline.basis(rangeval = c(850, 1050), norder = 4,
    breaks = c(850, 900, 950, 1050))

  fdalrn = setHyperPars(fdalrn, basis.x = basis.x)
  fdalrn = setHyperPars(fdalrn, family = "binomial()")


  rdesc = makeResampleDesc("RepCV", fold = 5, reps = 10)


  r = resample(fdalrn, tsk1, rdesc, measures = list(rmse))
  r
})
