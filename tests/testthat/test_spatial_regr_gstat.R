# testthat::test_file("./tests/testthat/test_spatial_gstat.R")
# devtools::test(filter = "gstat")
# devtools::document()
# devtools::load_all()
# devtools::test(filter = "spatial_regr_gstat")
# https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio
# sink(NULL)
# https://github.com/r-lib/devtools/issues/1675 - Error in x[[method]](...) : attempt to apply non-function
# https://stackoverflow.com/questions/50083521/error-in-xmethod-attempt-to-apply-non-function-in-testthat-test-when
# https://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on

context("regr_gstat")
test_that("regr_gstat", {
  requirePackagesOrSkip("gstat", default.method = "load")

  parset.list = list(
     list(id = "ordinary_kriging_manual",
      psill = 1, model.manual = "Sph", range = 900, nugget = 2),
     list(id = "ordinary_kriging_auto",
      model.auto = c('Sph','Exp','Gau', 'Mat')) ,# was psill
     list(id = "inverse_distance_weighted"),
    list(id = "trend_surfaces_degree_1", degree = 1),
    list(id = "trend_surfaces_degree_2", degree = 2),
    list(id = "trend_surfaces_degree_3", degree = 3)
  )

  old.predicts.list = list()

  # https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = meuse.formula, data = meuse.train, locations = ~x+y)
    pars = c(pars, parset)

    set.seed(getOption("mlr.debug.seed"))

    if (!is.null(pars$model.manual) || !is.null(pars$model.auto)) {
      pars$formula = update(pars$formula, .~.-y-x)
      #browser()
      # calculate sample variogram
      v = gstat::variogram(
        object = pars$formula,
        data = pars$data,
        locations = pars$locations
      )
      # Check for auto-fitting
      if (!is.null(pars$model.auto)) {
        pars$psill = pars$model.auto
      }
      # generate the variogram model (https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/vgm)
      model = gstat::vgm(
        psill = pars$psill,
        model = pars$model.manual,
        range = pars$range,
        nugget = pars$nugget
      )
      # fit the variogram model
      fit = gstat::fit.variogram(
        object = v,
        model = model
      )
      pars = list(formula = pars$formula, data = pars$data, locations = pars$locations, model = fit)
    } else {
        pars$formula = update(pars$formula, .~1) # https://stackoverflow.com/questions/18070131/update-formula-in-r
        pars$model = NULL
    }
    #browser()
    g = do.call(
      gstat::gstat,
      pars
    )
    p = predict(g, newdata = meuse.test)
    old.predicts.list[[i]] = p[,3]
  }
  #browser()
  #testSimpleParsets(t.name = "regr.gstat", df = meuse.task$env$data, target = meuse.target,
  testSimpleParsets(t.name = "regr.gstat", df = meuse.df, target = meuse.target,
    train.inds = meuse.train.inds, old.predicts.list = old.predicts.list, parset.list = parset.list)
})
