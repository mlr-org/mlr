# add dataframe extra attributes and append new class attribute
makeFDAData = function(df, fd.features, fd.grids) {
  attr(df,"fd.features") = fd.features
  attr(df,"fd.grids") = fd.grids
  addClasses(df, "fda.data.frame")
}

