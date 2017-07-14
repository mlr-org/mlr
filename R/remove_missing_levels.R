#' @title remove_missing_levels
#' @description Accounts for missing factor levels only present in test data
#' but not in train data by setting levels to NA
#'
#' @importFrom stringi stri_split stri_replace_all
#' @importFrom BBmisc warningf
#'
#' @param fit fitted model on training data
#'
#' @param test_data data to make predictions for
#'
#' @return data.frame with matching factor levels to fitted model
#'
#' @keywords internal
#'
#' @export
remove_missing_levels = function(fit, test_data) {

  # https://stackoverflow.com/a/39495480/4185785

  # drop empty factor levels in test data
  test_data = as.data.frame(droplevels(test_data))

  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors = stri_replace_all_regex(names(unlist(fit$learner.model$contrasts)),
                                     "[-^0-9]|as.factor|\\(|\\)", "")
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }

    factor_levels = unlist(lapply(fit$contrasts, function(x)
      outer(rownames(x), colnames(x), paste, sep = ":")))
    factor_levels = stri_split(factor_levels, ":", simplify = TRUE)[, 1]

    model_factors = as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors = stri_replace_all_regex(names(unlist(fit$learner.model$xlevels)),
                                     "[-^0-9]|as.factor|\\(|\\)", "")
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    factor_levels = unname(unlist(fit$learner.model$xlevels))
    model_factors = as.data.frame(cbind(factors, factor_levels))
  }

  # Select column names in test data that are factor predictors in
  # trained model
  predictors = names(test_data[names(test_data) %in% factors])

  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA

  for (i in 1:length(predictors)) {
    found = test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var = predictors[i]
      # set to NA
      test_data[!found, predictors[i]] = NA
      # drop empty factor levels in test data
      test_data = droplevels(test_data)
      # issue warning to console
      catf("\nSetting missing levels in '%s', only present in test data but missing in train data, to 'NA'.",
                      var)
      catf("This affected %s of %s total observations in test data.",
           table(is.na(test_data))[[2]], nrow(test_data))
    }
  }
  return(test_data)
}
