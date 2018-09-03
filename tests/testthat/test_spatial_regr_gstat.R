# testthat::test_file("./tests/testthat/test_spatial_gstat.R")
# devtools::test(filter = "gstat")
# devtools::document()
# devtools::load_all()
# https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio
# sink(NULL)
# https://github.com/r-lib/devtools/issues/1675 - Error in x[[method]](...) : attempt to apply non-function
# https://stackoverflow.com/questions/50083521/error-in-xmethod-attempt-to-apply-non-function-in-testthat-test-when
# https://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on

context("regr_gstat")
test_that("regr_gstat", {
  requirePackagesOrSkip("gstat", default.method = "load")

  parset.list = list(
    list(id = "inverse_distance_weighted"),
    list(id = "trend_surfaces_degree_1", degree = 1),
    list(id = "trend_surfaces_degree_2", degree = 2),
    list(id = "trend_surfaces_degree_3", degree = 3),
    list(id = "ordinary_kriging_manual",
      psill = 1, model = "Sph", range = 900, nugget = 2),
    list(id = "ordinary_kriging_auto",
      psill = c('Sph','Exp','Gau', 'Mat'))
  )

  old.predicts.list = list()

  # https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict

  for (i in 1:length(parset.list)) {
    browser()
    parset = parset.list[[i]]
    pars = list(formula = meuse.formula, data = meuse.train)
    pars = c(pars, parset)
    pars$locations = ~x+y

    # if (is.null(parset$degree) && is.null(parset$psill)) {
    #
    #
    # }

    set.seed(getOption("mlr.debug.seed"))

    if (!is.null(pars$psill)) {
      # build the samples variogram
      v = gstat::variogram(object = pars$formula, data = pars$data, locations = pars$locations)
      # fit the variogram model
      fit = gstat::fit.variogram(object = v, gstat::vgm(psill = pars$psill, model = pars$model,
        range = pars$range, nugget = pars$nugget))
      pars = list(formula = pars$formula, data = pars$data, locations = pars$locations, model = fit)
    }
    m = do.call(gstat::gstat, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = meuse.test)
    old.predicts.list[[i]] = p[,3]
  }
  browser()
  #testSimpleParsets(t.name = "regr.gstat", df = meuse.task$env$data, target = meuse.target,
  testSimpleParsets(t.name = "regr.gstat", df = meuse.df, target = meuse.target,
    train.inds = meuse.train.inds, old.predicts.list = old.predicts.list, parset.list = parset.list)
})
