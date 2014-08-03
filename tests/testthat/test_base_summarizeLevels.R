testsummarizeLevels <- function(){
  data <- data.frame(num = as.numeric(1:6),fac = as.factor(1:6),int = as.integer(1:6))
  data2 <- data.frame(cha = as.character(1:6),cha2 = 3:8)
  data3 <- data.frame(fac = as.factor(c(rep(1,1000),rep(2,100000))),fac2 = as.integer(rep(4,101000)))
  data4 <- data.frame(fac1 = as.factor(1))
  
  checkException(summarizeLevels(data,"num"),silent=T)
  checkException(summarizeLevels(data,"int"),silent=T)
  checkEquals(as.numeric(summarizeLevels(data)[[1]]), rep(1, 6))
  checkEquals(as.numeric(summarizeLevels(data2)[[1]]),rep(1,6))
  checkEquals(as.numeric(summarizeLevels(data3)[[1]]),c(1000,100000))
  checkEquals(as.numeric(summarizeLevels(data4)[[1]]),1)
}


