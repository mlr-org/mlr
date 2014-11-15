mlr: Machine Learning in R
==========================

* [Offical CRAN release site](http://cran.r-project.org/web/packages/mlr/)
* [Detailed Tutorial](http://berndbischl.github.io/mlr/tutorial/html/) ([Download](http://berndbischl.github.io/mlr/tutorial/tutorial.zip))
* [R Documentation in HTML](http://www.rdocumentation.org/packages/mlr)
* Install the development version

    ```splus
    devtools::install_github("berndbischl/mlr")
    ```

* [Further installation instructions](https://github.com/tudo-r/PackagesInfo/wiki/Installation-Information)
* Travis CI: [![Build Status](https://travis-ci.org/berndbischl/mlr.png)](https://travis-ci.org/berndbischl/mlr)


Introduction
============

R does not define a standardized interface for all its machine learning algorithms. Therefore, for any
non-trivial experiments, you need to write lengthy, tedious and error-prone wrappers to call the different
algorithms and unify their respective output. Additionally you need to implement infrastructure to resample
your models, optimize hyperparameters, select features, cope with pre- and post-processing of data and
compare models in a statistically meaningful way.
As this becomes computationally expensive, you might want to parallelize your experiments as well. This
often forces users to make crummy trade-offs in their experiments due to time constraints or lacking expert
programming skills. **mlr** provides this infrastructure so that you can focus on your experiments!
The framework currently focuses on supervised methods like classification, regression and survival analysis and
their corresponding evaluation and optimization. It is written in a way that you can extend it yourself or
deviate from the implemented convenience methods and construct your own complex experiments or algorithms.

Features
========

* Clear S3 interface to R classification and regression methods
* Easy extension mechanism through S3 inheritance
* Abstract description of learners and tasks by properties
* Parameter system for learners to encode data types and constraints
* Many convenience methods and generic building blocks for your
  machine learning experiments
* Resampling like bootstrapping, cross-validation and subsampling
* Easy hyperparameter tuning using different optimization strategies
* Variable selection with filters and wrappers
* Parallelization is built-in
* Extension points to integrate your own stuff
* Unit-testing
* Possibility to fit, predict, evaluate and resample models
* Tune hyper-parameters of a learner with different optimization algorithms
* Feature selection with filters and wrappers
* Combine different processing steps to a complex data mining chain; enables nested resampling of optimized models


News
====
Most NEWS regarding extensions and changes of the packages can be accessed here for the
[release](http://cran.r-project.org/web/packages/mlr/NEWS) and here for the
[devel](https://github.com/berndbischl/mlr/blob/master/NEWS) version on Github.

* 2014-10-28:
  * mlr 2.2 released to CRAN.
  * The popular Java tool WEKA uses mlr in its [RPlugin](http://weka.sourceforge.net/packageMetaData/RPlugin/index.html).
* 2014-10-18:
  * We have improved the tutorial A LOT. It is also based on mkdocs and knitr now. Still fixing minor things and extending a bit. Take a look yourself.
  * We might refactor the internal handling and OO structure of tasks a bit, so even more natural indexing and operations are possible. Michel is currently working on this in a branch.
  * I will talk about mlr in connection with the [OpenML](http://www.openml.org) project next week. If you don't know this, take a look, a very cool initiative by Joaquin Vanschoren. We are developing a connector to R in general which also covers mlr [here](https://github.com/openml/r).


Get in Touch: Tracker and Mailinglists
======================================
We have used mailing lists in the past and there still is (mainly for devel)

https://groups.google.com/forum/?hl=de#!forum/mlr-devel

mlr-devel@googlegroups.com

But we strongly prefer to use the Github issue tracker now, because it is simply easier to handle and we do not forget your issues as easily as when they come in emails.
We use this ourselves a lot and mainly communicate there, not that much on the mailing list.
We also do not hate beginners and it is perfectly valid to mark a issue as "Question".
But please don't forget that all of us work in academia and put a lot of work into this project, simply because we like it, not because we are specifically paid for it.

If you are interested in seeing a new feature, also use the tracker.
We also welcome pull requests or new developers.

For everything else the maintainer Bernd Bischl can be reached here: bernd_bischl@gmx.net.
He (=me) is sometimes busy, so please use the other channels for appropriate stuff first, so you get quicker responses ;-)

