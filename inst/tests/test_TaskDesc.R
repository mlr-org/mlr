context("TaskDesc")

test_that("TaskDesc", {
	ct = makeClassifTask(target="Class", binaryclass.df, id="mytask", positive="M")
	expect_equal(ct$task.desc$id, "mytask")	
	expect_equal(ct$task.desc$positive, "M")	
	expect_equal(ct$task.desc$negative, "R")

	ct = makeClassifTask(target="Species", multiclass.df, id="mytask2")
	expect_equal(ct$task.desc$id, "mytask2")	
	expect_true(is.na(ct$task.desc$positive))
	expect_true(is.na(ct$task.desc$negative))
	
	rt = makeRegrTask(target="medv", regr.df, id="mytask3") 
	expect_equal(rt$task.desc$id, "mytask3")	
	expect_true(is.na(rt$task.desc$positive))
	expect_true(is.na(rt$task.desc$negative))
  
  expect_equal(multiclass.task$task.desc$size, 150) 
  expect_equal(sum(multiclass.task$task.desc$n.feat), 4)  
  expect_equal(multiclass.task$task.desc$n.feat[["numerics"]], 4)  
  expect_equal(multiclass.task$task.desc$n.feat[["factors"]], 0)  
  expect_equal(multiclass.task$task.desc$has.missings, F)  
  expect_equal(multiclass.task$task.desc$type, "classif") 
  expect_equal(multiclass.task$task.desc$class.levels, c("setosa", "versicolor", "virginica"))  
  
  # check missing values
  df = multiclass.df
  df[1,1] = as.numeric(NA)
  ct = makeClassifTask(target="Species", data=df)
  expect_equal(ct$task.desc$has.missings, T) 
  
  ct = makeClassifTask(target=binaryclass.target, data=binaryclass.df)
  expect_equal(ct$task.desc$size, 208)  
  expect_equal(sum(ct$task.desc$n.feat), 60)  
  expect_equal(ct$task.desc$n.feat[["numerics"]], 60)  
  expect_equal(ct$task.desc$n.feat[["factors"]], 0)  
  expect_equal(ct$task.desc$has.missings, F) 
  expect_equal(ct$task.desc$type, "classif")  
  expect_equal(ct$task.desc$class.levels, c("M", "R"))  
  
  expect_equal(regr.task$task.desc$size, 506) 
  expect_equal(sum(regr.task$task.desc$n.feat), 13) 
  expect_equal(regr.task$task.desc$n.feat[["numerics"]], 12)  
  expect_equal(regr.task$task.desc$n.feat[["factors"]], 1)  
  expect_equal(regr.task$task.desc$has.missings, F)  
  expect_equal(regr.task$task.desc$type, "regr")  
  expect_true(is.na(regr.task$task.desc$class.levels)) 
})