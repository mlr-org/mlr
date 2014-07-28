pkgs <- c(
  "devtools",
  "roxygen2",
  "testthat",
  "checkmate",
  "abind", 
  "BBmisc", 
  "codetools", 
  "ada", 
  "adabag", 
  "cmaes",
  "CoxBoost",
  "crs",
  "DiceKriging", 
  "DiscriMiner",
  "e1071",
  "earth", 
  "FNN",
  "FSelector",
  "gbm",
  "GenSA",
  "glmnet",
  "kernlab", 
  "kknn", 
  "klaR",
  "LiblineaR",
  "lqa", 
  "mboost", 
  "mco", 
  "mda",
  "mRMRe",
  "nnet", 
  "ParamHelpers", 
  "party",
  "penalized", 
  "pls",
  "randomForest",
  "randomForestSRC",
  "robustbase",
  "rpart", 
  "rrlda",
  "ROCR",
  "pROC",
  "rsm",
  "RWeka",
  "stepPlr"
)

# Determine missing packages 
installed.pkgs <- rownames(installed.packages())
missing.pkgs <- setdiff(pkgs, installed.pkgs)

if(length(missing.pkgs) > 0) {
  cat("Installing the following packages:\n")
  print(missing.pkgs)
  install.packages(missing.pkgs, dep = TRUE, repos="http://cran.at.r-project.org")
} else {
  cat("All packages installed!\n")
}


