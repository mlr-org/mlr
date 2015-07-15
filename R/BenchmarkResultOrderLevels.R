# order levels of task.ids in a BenchmarkResult or a similar data.frame
# usefull for plotting in ggplot2

orderBMRTasks = function(bmr, df = NULL, order.tsks) {
  
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.tsks, len = length(getBMRTaskIds(bmr)))
  
  if (is.null(df))
    df = as.data.frame(bmr)
  assertClass(df, "data.frame")
  assertCharacter(order.tsks)
  assertSetEqual(order.tsks, getBMRTaskIds(bmr), ordered = FALSE) 

  df$task.id = factor(df$task.id, order.tsks)
  return(df)
}



# order levels of learner.ids of a BenchmarkResult or similar data.frame
# usefull for plotting in ggplot2
orderBMRLrns = function(bmr, df = NULL, order.lrns){
  
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.lrns, len = length(getBMRLearnerIds(bmr)))
  
  # create df and/or getLearnerIds
  if (is.null(df))
    df = as.data.frame(bmr)
    
  assertCharacter(order.lrns)
  assertSetEqual(order.lrns, getBMRLearnerIds(bmr), ordered = FALSE) 
  df$learner.id = factor(df$learner.id, order.lrns)
  return(df)
}
