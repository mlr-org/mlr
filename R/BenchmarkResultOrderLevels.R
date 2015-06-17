# Order Levels of task.ids of a BenchmarkResult
# Usefull for plotting in ggplot2
orderBMRTasks = function(bmr,df=NULL,orderTsks){
  if (is.null(df)){df = as.data.frame(bmr)}
  if (is.numeric(orderTsks)){
    orderTsks= getBMRTaskIds(bmr)[orderTsks]
  }
  assertSetEqual(orderTsks, getBMRTaskIds(bmr), ordered = FALSE) 
  df$task.id = factor(df$task.id, orderTsks)
  return(df)
}

# Order Levels of learner.ids of a BenchmarkResult
# Usefull for plotting in ggplot2
orderBMRLrns = function(bmr,df=NULL,orderLrns){
  if (is.null(df)){df = as.data.frame(bmr)}
  if (is.numeric(orderLrns)){
    orderLrns = getBMRLearnerIds(bmr)[orderLrns]
  }
  assertSetEqual(orderLrns, getBMRLearnerIds(bmr), ordered = FALSE) 
  df$learner.id = factor(df$learner.id, orderLrns)
  return(df)
}
