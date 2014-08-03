testMergeFactorLevelsBySize = function() {
  f1 = as.factor(rep(c("a", "b", "c"), times=c(85, 10, 5)))
  f2 = 1:65

  g1 = mergeFactorLevelsBySize(f1)
  checkEquals(levels(g1), c("a", "b", "c"))
  g1 = mergeFactorLevelsBySize(f1, min.perc=0.04)
  checkEquals(levels(g1), c("a", "b", "c"))
  g1 = mergeFactorLevelsBySize(f1, min.perc=0.06)
  checkEquals(levels(g1), c("a", "merged"))
  checkEquals(sum(g1 == "merged"), 15)
  
  checkException(mergeFactorLevelsBySize(f1, min.perc=2))
  checkException(mergeFactorLevelsBySize(f2))
  checkException(mergeFactorLevelsBySize(f1, new.name="a"))
}
