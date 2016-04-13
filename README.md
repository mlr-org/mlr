![mlr](http://mlr-org.github.io/mlr-tutorial/images/mlrLogo_blue_141x64.png): Machine Learning in R
==========================
[![Build Status](https://travis-ci.org/mlr-org/mlr.svg?branch=master)](https://travis-ci.org/mlr-org/mlr)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mlr)](https://cran.r-project.org/web/packages/mlr)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/mlr)](https://cran.rstudio.com/web/packages/mlr/index.html)

* [Offical CRAN release site](http://cran.r-project.org/web/packages/mlr/)
* Detailed Tutorial:
    * [mlr release](https://mlr-org.github.io/mlr-tutorial/release/html/) ([online](https://mlr-org.github.io/mlr-tutorial/release/html/), [download for offline usage](https://mlr-org.github.io/mlr-tutorial/release/mlr_tutorial.zip))
    * [mlr devel](https://mlr-org.github.io/mlr-tutorial/devel/html/) ([online](https://mlr-org.github.io/mlr-tutorial/devel/html/), [download for offline usage](https://mlr-org.github.io/mlr-tutorial/devel/mlr_tutorial.zip))
* [R Documentation in HTML](http://rpackages.ianhowson.com/cran/mlr/)
* Internal [Jenkins test infrastructure](https://hagakure.cs.ubc.ca:2893/view/mlr/) - only for developers.
* Install the development version

    ```splus
    devtools::install_github("mlr-org/mlr")
    ```
* [Further installation instructions](https://github.com/rdatsci/PackagesInfo/wiki/Installation-Information)
* [Ask a question about mlr on Stackoverflow](https://stackoverflow.com/questions/tagged/mlr)


Introduction
============

R does not define a standardized interface for all its machine learning
algorithms. Therefore, for any non-trivial experiments, you need to write
lengthy, tedious and error-prone wrappers to call the different algorithms and
unify their respective output. Additionally you need to implement infrastructure
to resample your models, optimize hyperparameters, select features, cope with
pre- and post-processing of data and compare models in a statistically
meaningful way. As this becomes computationally expensive, you might want to
parallelize your experiments as well. This often forces users to make crummy
trade-offs in their experiments due to time constraints or lacking expert
programming skills. **mlr** provides this infrastructure so that you can focus
on your experiments! The framework provides supervised methods like
classification, regression and survival analysis along with their corresponding
evaluation and optimization methods, as well as unsupervised methods like clustering. It
is written in a way that you can extend it yourself or deviate from the
implemented convenience methods and construct your own complex experiments or
algorithms.

Features
========

* Clear S3 interface to R classification, regression, clustering and survival analysis methods
* Possibility to fit, predict, evaluate and resample models
* Easy extension mechanism through S3 inheritance
* Abstract description of learners and tasks by properties
* Parameter system for learners to encode data types and constraints
* Many convenience methods and generic building blocks for your
  machine learning experiments
* Resampling methods like bootstrapping, cross-validation and subsampling
* Extensive visualizations for e.g. ROC curves, predictions and partial
  predictions
* Benchmarking of learners for multiple data sets
* Easy hyperparameter tuning using different optimization strategies, including
  potent configurators like iterated F-racing (irace) or sequential model-based
  optimization
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

*If you like the package, please "star" it on Github.*


News
====
Most NEWS regarding extensions and changes of the packages can be accessed here for the
[release](http://cran.r-project.org/web/packages/mlr/NEWS) and here for the
[devel](https://github.com/mlr-org/mlr/blob/master/NEWS) version on Github.

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
  * I (Bernd) was pretty busy as I had to change cities and workplaces. I now head the Computational Statistics Group at LMU Munich. More importantly, this resulted in me not taking care of requests and issues as much as I wanted during the last weeks. Apologies and hopefully I have more time from now on.
  * mlr got not one, but three project slots in Google Summer of Code 2015. Many thanks to the R Foundation, Google and all students who applied with exciting proposals. Best of luck to Tong, Zach and Pascal, who will work on SVM ensembles, mlr's visualization system and better hyperparameter / tuning options.
* 2015-02-17:
  * **We have been informed that our tutorial "Applied Machine Learning and Efficient Model Selection with mlr" has been accepted for [useR 2015](http://user2015.math.aau.dk/) in Aarlborg. Hoping to meet all of you there in June!**
* 2015-02-04:
  * mlr 2.3 released to CRAN.
* 2014-10-28:
  * mlr 2.2 released to CRAN.
  * The popular Java tool WEKA uses mlr in its [RPlugin](http://weka.sourceforge.net/packageMetaData/RPlugin/index.html).
* 2014-10-18:
  * We have improved the tutorial A LOT. It is also based on mkdocs and knitr now. Still fixing minor things and extending a bit. Take a look yourself.
  * We might refactor the internal handling and OO structure of tasks a bit, so even more natural indexing and operations are possible. Michel is currently working on this in a branch.
  * I will talk about mlr in connection with the [OpenML](http://www.openml.org) project next week. If you don't know this, take a look, a very cool initiative by Joaquin Vanschoren. We are developing a connector to R in general which also covers mlr [here](https://github.com/openml/r).


Talks and Videos
================
* [Video](https://www.youtube.com/watch?v=rzjkT1uLNi4) of Bernd's "mlr + OpenML" talk at OpenML workshop 2014
* [Video](https://www.youtube.com/watch?v=d1PnFiN6nOQ) of Zach's International Methods Colloquim talk 2015 "Data Mining as Exploratory Data Analysis"


Get in Touch
============

Please use the issue tracker for problems, questions and feature requests.
Don't email in most cases, as we forget these mails.

We also do not hate beginners and it is perfectly valid to mark an issue as "Question".

Please don't forget that all of us work in academia and put a lot of work into this project, simply because we like it, not because we are specifically paid for it.

We also welcome pull requests or new developers.
Just make sure that you have a glance at our [**mlr** coding guidelines](https://github.com/mlr-org/mlr/wiki/mlr-Coding-Guidelines) before.

For everything else the maintainer Bernd Bischl can be reached here: bernd_bischl@gmx.net.
He (=me) is sometimes busy, so please use the other channels for appropriate stuff first, so you get quicker responses ;-)
