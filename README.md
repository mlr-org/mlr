# Machine Learning in R <img src="man/figures/logo_navbar.png" align="right" />

[![Build Status](https://travis-ci.org/mlr-org/mlr.svg?branch=master)](https://travis-ci.org/mlr-org/mlr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/mlr)](https://cran.r-project.org/package=mlr)
[![cran checks](https://cranchecks.info/badges/worst/mlr)](https://cran.r-project.org/web/checks/check_results_mlr.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlr)](https://cran.rstudio.com/web/packages/mlr/index.html)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr-blue.svg)](https://stackoverflow.com/questions/tagged/mlr)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr)](https://cran.r-project.org/package=mlr)

* [CRAN release site](https://CRAN.R-project.org/package=mlr)
* [Online tutorial](https://mlr.mlr-org.com/index.html)
* [Cheatsheet](https://github.com/mlr-org/mlr/blob/master/addon/cheatsheet/MlrCheatsheet.pdf)
* [Changelog](https://mlr.mlr-org.com/news/index.html)

**We are actively working on [mlr3](https://github.com/mlr-org/mlr3) as a successor of *mlr*. This implies that we have less time to reply to *mlr* issues.**

* Stackoverflow: [mlr](https://stackoverflow.com/questions/tagged/mlr)
* [Slack](https://mlr-org.slack.com/)
* [Blog](https://mlr-org.com/)

### Installation

**Release**

```r
install.packages("mlr")
```

**Development**

```R
remotes::install_github("mlr-org/mlr")
```

# Citing _mlr_ in publications
Please cite our [JMLR paper](http://jmlr.org/papers/v17/15-066.html) [[bibtex](http://www.jmlr.org/papers/v17/15-066.bib)].

Some parts of the package were created as part of other publications.
If you use these parts, please cite the relevant work appropriately.
An overview of all _mlr_ related publications can be found [here](https://mlr.mlr-org.com/articles/tutorial/mlr_publications.html).

# Introduction

R does not define a standardized interface for its machine-learning algorithms.
Therefore, for any non-trivial experiments, you need to write lengthy, tedious and error-prone wrappers to call the different algorithms and unify their respective output.

Additionally you need to implement infrastructure to
- resample your models
- optimize hyperparameters
- select features
- cope with pre- and post-processing of data and compare models in a statistically meaningful way.

As this becomes computationally expensive, you might want to parallelize your experiments as well. This often forces users to make crummy trade-offs in their experiments due to time constraints or lacking expert programming skills.

_mlr_ provides this infrastructure so that you can focus on your experiments!
The framework provides supervised methods like classification, regression and survival analysis along with their corresponding evaluation and optimization methods, as well as unsupervised methods like clustering.
It is written in a way that you can extend it yourself or deviate from the implemented convenience methods and construct your own complex experiments or algorithms.

Furthermore, the package is nicely connected to the [**OpenML**](https://github.com/openml/openml-r) R package and its [online platform](https://www.openml.org/), which aims at supporting collaborative machine learning online and allows to easily share datasets as well as machine learning tasks, algorithms and experiments in order to support reproducible research.

# Features

* Clear **S3** interface to R **classification, regression, clustering and survival** analysis methods
* Abstract description of learners and tasks by properties
* Convenience methods and generic building blocks for your machine learning experiments
* Resampling methods like **bootstrapping, cross-validation and subsampling**
* Extensive visualizations (e.g. ROC curves, predictions and partial predictions)
* Simplified benchmarking across data sets and learners
* Easy hyperparameter tuning using different optimization strategies, including potent configurators like
  - **iterated F-racing (irace)**
  - **sequential model-based optimization**
* **Variable selection with filters and wrappers**
* Nested resampling of models with tuning and feature selection
* **Cost-sensitive learning, threshold tuning and imbalance correction**
* Wrapper mechanism to extend learner functionality in complex ways
* Possibility to combine different processing steps to a complex data mining chain that can be jointly optimized
* **OpenML** connector for the Open Machine Learning server
* Built-in **parallelization**
* **Detailed tutorial**

# Miscellaneous

Simple usage questions are better suited at Stackoverflow using the [mlr](https://stackoverflow.com/questions/tagged/mlr) tag.

Please note that all of us work in academia and put a lot of work into this project - simply because we like it, not because we are paid for it.

New development efforts should go into [_mlr3_](https://github.com/mlr-org/mlr3).
We have a [developer guide](https://www.notion.so/mlrorg/Developer-Guide-65e60b651a2e44e89a36b72df92158ea) and our own coding style which can easily applied by using the `mlr_style` from the [styler](https://github.com/r-lib/styler) package.

# mlr-tutorial

Please read [here](https://www.notion.so/mlrorg/mlr-Tutorial-b71444fe979c4a8cafe91e10e7f81d79) if you want to contribute to the Online Manual.
