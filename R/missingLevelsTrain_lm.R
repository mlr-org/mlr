#' @export
missingLevelsTrain.lm = function(.model, .test.data) {

  # https://stackoverflow.com/a/39495480/4185785

  # drop empty factor levels in test data
  .test.data = as.data.frame(droplevels(.test.data))

  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  # Obtain factor predictors in the model and their levels
  factors = stri_replace_all_regex(names(unlist(.model$learner.model$xlevels)),
                                   "[-^0-9]|as.factor|\\(|\\)", "")

  if (length(factors) == 0) {
    return(.test.data)
  }
  factor.levels = unname(unlist(.model$learner.model$xlevels))
  model.factors = as.data.frame(cbind(factors, factor.levels))


  # Select column names in test data that are factor predictors in
  # trained model
  predictors = names(.test.data[names(.test.data) %in% factors])

  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA

  for (i in seq_len(length(predictors))) {
    found = .test.data[, predictors[i]] %in% model.factors[
      model.factors$factors == predictors[i], ]$factor.levels
    if (any(!found)) {
      # track which variable
      var = predictors[i]
      .test.data[!found, predictors[i]] = NA
      .test.data = droplevels(.test.data)
      # issue warning to console
      catf("Setting missing levels in '%s', only present in test data but missing in train data, to 'NA'.",
           var)
      catf("This affected %s of %s total observations in test data.",
           table(is.na(.test.data))[[2]], nrow(.test.data))
    }
  }
  return(.test.data)
}
