testConvertColumNames = function() {
  data = data.frame(x=1:2, y=1:2, z=1:2)
  colnames(data) = c("x", "x+y", "z()")
  data2 = convertColumnNames(data)
  checkEquals(colnames(data2), c("x", "x43y", "z4041"))
}
