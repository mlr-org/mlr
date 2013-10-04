context("multipoint multicrit")

test_that("multipoint multicrit", {
  f = generate_branin_function()
  ps = makeNumericParamSet(lower=lower_bounds(f), upper=upper_bounds(f))
  lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")
  
  #FIXME how can we test this better?
  for (obj in c("ei.dist", "mean.se", "mean.se.dist")) {
    for (dist in c("nearest.better", "nearest.neighbor")) {
      for (sel in c("hypervolume", "crowdingdist")) {
        
        ctrl = makeMBOControl(init.design.points=10, iters=1, propose.points=4, 
          multipoint.method="multicrit",
          multipoint.multicrit.objective=obj,
          multipoint.multicrit.dist=dist,
          multipoint.multicrit.sel=sel,
          multipoint.multicrit.maxit=30
        )
        
        res = mbo(makeMBOFunction(f), par.set=ps, learner=lrn, control=ctrl)
        
        gap = res$y - global_minimum(f)$value
        print(gap)
        #expect_true(gap < 0.1)
      }
    }
  }
  
})
