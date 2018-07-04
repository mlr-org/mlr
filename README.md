![mlr](https://raw.githubusercontent.com/mlr-org/mlr/master/man/figures/logo_navbar.png) Machine Learning in R
==========================
[![Build Status](https://travis-ci.org/mlr-org/mlr.svg?branch=master)](https://travis-ci.org/mlr-org/mlr)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mlr)](https://CRAN.R-project.org/package=mlr)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/mlr)](https://cran.rstudio.com/web/packages/mlr/index.html)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr-blue.svg)](https://stackoverflow.com/questions/tagged/mlr)

* [Offical CRAN release site](https://CRAN.R-project.org/package=mlr)
* [mlr cheatsheet](https://github.com/mlr-org/mlr/blob/master/vignettes/tutorial/cheatsheet/MlrCheatsheet.pdf)
* Detailed Tutorial:
    * [Main: Online as HTML (devel)](https://mlr-org.github.io/mlr/) (For the HTML docs of the current CRAN version, replace `devel` by `release` in the URL.)
    * [Download PDF for offline usage (CRAN)](https://github.com/mlr-org/mlr/blob/tutorial_pdf_release/vignettes/tutorial/release/pdf/mlr-tutorial_release.pdf))
    * [Download PDF for offline usage (devel)](https://github.com/mlr-org/mlr/blob/tutorial_pdf_dev/vignettes/tutorial/devel/pdf/mlr-tutorial_dev.pdf))
* [R Documentation in HTML](https://mlr-org.github.io/mlr/reference/index.html)
* Install the development version

```R
devtools::install_github("mlr-org/mlr")
```

* [Further installation instructions](https://github.com/rdatsci/PackagesInfo/wiki/Installation-Information)
* [Ask a question about mlr on Stackoverflow](https://stackoverflow.com/questions/tagged/mlr)
* [We are on Slack](https://mlr-org.slack.com/) (Request invitation: code{at}jakob-r.de)
* [We have a blog on mlr](https://mlr-org.github.io/)
* A list of possible enhancements to mlr is available on the [wiki](https://github.com/mlr-org/mlr/wiki/List-of-Possible-Enhancements-to-mlr) - contributors welcome!
* We are in the top 20 of the most starred R packages on Github, as reported by [metacran](http://www.r-pkg.org/starred).
* **If you like the package, please "star" it on Github.**

mlr - How to Cite and Citing Publications
=========================================

Please cite our [JMLR paper](http://jmlr.org/papers/v17/15-066.html) [[bibtex](http://www.jmlr.org/papers/v17/15-066.bib)].

Some parts of the package were created as part of other publications.
If you use these parts, please cite the relevant work appropriately. 
An overview of all mlr related publications can be found [here](http://mlr-org.github.io/mlr/articles/tutorial/devel/mlr_publications.html).

A list of publications that cite mlr can be found in the [wiki](https://github.com/mlr-org/mlr/wiki/Publications-that-use-mlr).


Introduction
============

R does not define a standardized interface for all its machine learning algorithms. 
Therefore, for any non-trivial experiments, you need to write lengthy, tedious and error-prone wrappers to call the different algorithms and unify their respective output. 

Additionally you need to implement infrastructure to resample your models, optimize hyperparameters, select features, cope with pre- and post-processing of data and compare models in a statistically meaningful way. 
As this becomes computationally expensive, you might want to parallelize your experiments as well. This often forces users to make crummy trade-offs in their experiments due to time constraints or lacking expert programming skills. 

**mlr** provides this infrastructure so that you can focus on your experiments! 
The framework provides supervised methods like classification, regression and survival analysis along with their corresponding evaluation and optimization methods, as well as unsupervised methods like clustering. 
It is written in a way that you can extend it yourself or deviate from the implemented convenience methods and construct your own complex experiments or algorithms. 

Furthermore, the package is nicely connected to the [**OpenML**](https://github.com/openml/openml-r) R package and its [online platform](https://www.openml.org/), which aims at supporting collaborative machine learning online and allows to easily share datasets as well as machine learning tasks, algorithms and experiments in order to support reproducible research.

Features
========

* Clear S3 interface to R classification, regression, clustering and survival analysis methods
* Possibility to fit, predict, evaluate and resample models
* Easy extension mechanism through S3 inheritance
* Abstract description of learners and tasks by properties
* Parameter system for learners to encode data types and constraints
* Many convenience methods and generic building blocks for your machine learning experiments
* Resampling methods like bootstrapping, cross-validation and subsampling
* Extensive visualizations for e.g. ROC curves, predictions and partial predictions
* Benchmarking of learners for multiple data sets
* Easy hyperparameter tuning using different optimization strategies, including potent configurators like iterated F-racing (irace) or sequential model-based optimization
* Variable selection with filters and wrappers
* Nested resampling of models with tuning and feature selection
* Cost-sensitive learning, threshold tuning and imbalance correction
* Wrapper mechanism to extend learner functionality in complex and custom ways
* Combine different processing steps to a complex data mining chain that can be jointly optimized
* OpenML connector for the Open Machine Learning server
* Extension points to integrate your own stuff
* Parallelization is built-in
* Unit-testing
* Detailed tutorial

News
====
Changes of the packages can be accessed in the [NEWS file](https://mlr-org.github.io/mlr/news/index.html) shipped with the package.

* 2018-03-10: mlr 2.12 released to CRAN.
* OpenML has been accepted as a tutorial at useR2017! Blog post is [here](http://mlr-org.github.io/OpenML-tutorial-at-useR/).
* 2017-03-15: mlr 2.11 released to CRAN. About 1000 smaller fixes and convenience extensions and functions. 
Larger new features are: Bayesian Optimization with [mlrMBO](https://github.com/mlr-org/mlrMBO), learner API now supports extracting OOB predictions.
* Our Bayesian Optimization framework [mlrMBO](https://github.com/mlr-org/mlrMBO) has FINALLY been released to CRAN.
Have a look at our paper [here](https://arxiv.org/abs/1703.03373).
* 2017-03-06: We had a week-long very productive mlr workshop at the LMU in Munich. 
Blog post is [here](http://mlr-org.github.io/mlr-workshop/). 
Many thanks to our sponsors that madeit possible that we could invite our American and Canadian friends.
* 2017-02-07: mlr 2.10 released to CRAN. About 1000 smaller fixes and convenience extensions and functions. 
Larger new features are: parallel irace tuning, learner API now supports feature importance extractions, better tables for confusion matrix and ROC tables, dummy learners with random or constant output for baseline comparisons and fuzzy matching to propose learner names when you mistyped a string.
* 2016-10-20:
  * The JMLR paper on mlr is finally published! See "How to cite" section above.
* 2016-08-24:
  * We have a (still smallish) [blog](https://mlr-org.github.io/) on things related to mlr.
* 2016-08-06:
  * We are hosting the first mlr workshop! It is in Palermo, and more like a sprint, as 11 core developers meet to get stuff done. 
Thanks to Giuseppe for organizing this! 
We are thinking about hosting the 2017 one in Munich and possibly opening this up more for the public. 
If you are interested in potentially joining, email Bernd.
* 2016-08-06:
  * mlr 2.9 released to CRAN. This is a pretty large release containing (among other things)
    * Many new measures and many new learners (also h2o and its deeplearning net)
    * More sophisticated algorithms for multilabel learning that can exploit label correlation.
    * The first batch of functions from Mason Gallo's very cool GSOC project for hyperparameter tuning visualization.
* 2016-02-13:
  * mlr 2.8 released to CRAN. Mainly a release with bug fix release and many minor improvements, also regarding the new ggplot2 release.
* 2015-11-26:
  * mlr 2.6 released to CRAN. A mini update, which was required as ViperCharts seems to be offline and we needed to switch off its test for CRAN. Contains also some fixes and new learners.
* 2015-11-21:
  * mlr 2.5 released to CRAN. In addition to other things and many cleanups and fixes, it features: a) Quite a few new learners b) plots and statistical tests to analyze benchmark results (e.g. Demsar) c) partial depedency plots for all models (which I like quite a lot) d) first support for multilabel classification.
* 2015-06-27:
  * Zach M Jones is doing great work in his current GSOC project to improve **mlr's visualization system** to explore models and data. Some of this is already in 2.4, much more is to come in upcoming 2.5. Here are some general remarks.
    * We try to use ggplot2 as a standard for static graphics you can use in papers and knitr docs. These plotting functions return ggplot2 objects you can later change with the usual "+"-operator.
    * We often provide ggvis versions of these plots, which are more interactive.
    * For each plot, there is a well-defined data layer / container object that contains the data necessary for the plot. This object is generated first and passed to the function that does the actual plotting. Pro: This enforces good design on our side. And if you dislike something in our plots, you can implement your own by using the container object, instead of doing everything from scratch. Con: You need to call 2 functions for a plot. We think the "Pros" are worth it.
  * mlr is becoming a **Github org**, because we are growing larger and need more structure. The tutorial is already on that org (and hopefully you don't even notice that), and we will migrate the whole project soon (hopefully also without many people noticing much).
  * The **tutorial is now continuously being built and checked with Travis CI**. It is also **versioned**, so we have subdirs 2.4, 2.5, and symlinks "devel" and "release". Before, we only had one version, and people got confused if we already explained stuff for the devel version, which was not on CRAN yet. (Julia and Lars put lots of work into all of this.)
* 2015-06-13:
  * mlr 2.4 released to CRAN.
* 2015-04-30:
  * I (Bernd) was pretty busy as I had to change cities and workplaces. 
  I now head the Computational Statistics Group at LMU Munich. 
  More importantly, this resulted in me not taking care of requests and issues as much as I wanted during the last weeks. 
  Apologies and hopefully I have more time from now on.
  * mlr got not one, but three project slots in Google Summer of Code 2015. 
  Many thanks to the R Foundation, Google and all students who applied with exciting proposals. 
  Best of luck to Tong, Zach and Pascal, who will work on SVM ensembles, mlr's visualization system and better hyperparameter / tuning options.
* 2015-02-17:
  * **We have been informed that our tutorial "Applied Machine Learning and Efficient Model Selection with mlr" has been accepted for [useR 2015](http://user2015.math.aau.dk/) in Aarlborg. Hoping to meet all of you there in June!**
* 2015-02-04:
  * mlr 2.3 released to CRAN.
* 2014-10-28:
  * mlr 2.2 released to CRAN.
  * The popular Java tool WEKA uses mlr in its [RPlugin](http://weka.sourceforge.net/packageMetaData/RPlugin/index.html).
* 2014-10-18:
  * We have improved the tutorial A LOT. It is also based on mkdocs and knitr now. Still fixing minor things and extending a bit. 
  Take a look yourself.
  * We might refactor the internal handling and OO structure of tasks a bit, so even more natural indexing and operations are possible. Michel is currently working on this in a branch.
  * I will talk about mlr in connection with the [OpenML](http://www.openml.org) project next week. 
  If you don't know this, take a look, a very cool initiative by Joaquin Vanschoren. 
  We are developing a connector to R in general which also covers mlr [here](https://github.com/openml/r).

Talks and Videos
================
* [Video](https://www.youtube.com/watch?v=rzjkT1uLNi4) of Bernd's "mlr + OpenML" talk at OpenML workshop 2014
* [Video](https://www.youtube.com/watch?v=d1PnFiN6nOQ) of Zach's International Methods Colloquium talk 2015 "Data Mining as Exploratory Data Analysis"

Get in Touch
============

Please use the issue tracker for problems, questions and feature requests.
Don't email in most cases, as we forget these mails.

We also do not hate beginners and it is perfectly valid to mark an issue as "Question".

Please don't forget that all of us work in academia and put a lot of work into this project, simply because we like it, not because we are specifically paid for it.

We also welcome pull requests or new developers.
Just make sure that you have a glance at our [**mlr** coding guidelines](https://github.com/mlr-org/mlr/wiki/mlr-Coding-Guidelines) before.

For everything else the maintainer Bernd Bischl can be reached via [mail](mailto:bernd_bischl@gmx.net).
He (=me) is sometimes busy, so please use the other channels for appropriate stuff first, so you get quicker responses ;-)

mlr-tutorial
====

With the start of v2.13 we switched from [mkdocs](https://github.com/mkdocs/mkdocs) to [pkgdown](https://github.com/r-lib/pkgdown).
With this change, all source files are now located in this repo under `vignettes/tutorial`.

**Modification of a tutorial section**:

If you want to modify/add a tutorial section, please follow these steps:

1. Open the respective source file, e.g. `task.Rmd`.
2. Follow the style guide while editing: 
     - Reference `mlr` functions as <function()>, e.g. `makeLearner()`.
     - Reference external functions as <package::function()>, e.g. `kernlab::ksvm()`.
     - Reference other tutorial pages with `<name_of_vignette>.html`, e.g. `[bagging](bagging.html)`.
     - Always start a new sentence with a new line. 
     - If you want to insert a paragraph, skip one line.
     - The most up-level header starts with `###` and is subsequently followed by `####` and `#####` etc.
     - Always insert *exactly one* empty line *before and after* a code chunk, header, figure or a table. 
     - Referencing images is a bit tricky since we need to ensure that they look good in both the HTML and PDF version.
       Put your image into `vignettes/tutorial/devel/pdf/img/` and see the examples in [resampling.Rmd](https://github.com/mlr-org/mlr/blob/master/vignettes/tutorial/devel/resampling.Rmd), [nested_resampling.Rmd](https://github.com/mlr-org/mlr/blob/master/vignettes/tutorial/devel/nested_resampling.Rmd) or [handling_of_spatial_data.Rmd](https://github.com/mlr-org/mlr/blob/master/vignettes/tutorial/devel/handling_of_spatial_data.Rmd).
3. Make sure that the `.Rmd` file is working on its own, i.e. compile it as a single file (preferably using `build_article("tutorial/devel/<vignette-name>")`) and see if everything works.
   Put required packages in the setup chunk at the beginning of the tutorial.
   
**Rendering the tutorial locally**:

If you want to view the complete `pkgdown` site locally, run `pkgdown::build_site(lazy = TRUE)`.
You don't have to render the complete site every time you change one tutorial. 
The `lazy = TRUE` argument ensures that only pages are rebuilt that have changed.
Also, if you have built the whole site once, you can just build the vignettes again by using `build_articles(lazy = TRUE)`.
More specific, if you are working on one vignette, you can run `build_article("tutorial/devel/<vignette-name>")`.
You do not need to pass the `.Rmd` extension when using `build_article()`.

Important: Do not commit any file in `docs/` as the rendering will be done by Travis!

**Adding a new section**:

Edit `_pkgdown.yml` and add the new section at the appropriate place.

**Issues and Pull Requests**:

If you want to open an issue or pull request that is related to `mlr-tutorial`, label it with `tutorial` and mention [jakob-r](https://github.com/jakob-r) or [pat-s](https://github.com/pat-s) if you need help.
