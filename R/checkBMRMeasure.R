# small arg checker for a selected measure for a BMR
# if NULL, the 1st measure in the BMR is returned
checkBMRMeasure = function(measure, bmr) {
  if (is.null(measure)) {
    measure = getBMRMeasures(bmr)[[1]]
  } else {
    assertClass(measure, "Measure")
    assertChoice(measure$id, getBMRMeasureIds(bmr))
  }
  return(measure)
}
