
ecr3vis: ecr3 visualization
===========================

<!-- badges: start -->

[![R-CMD-check](https://github.com/jakobbossek/ecr3vis/workflows/R-CMD-check/badge.svg)](https://github.com/jakobbossek/ecr3vis/actions)
<!-- badges: end -->
[![Status](https://img.shields.io/badge/Status-experimental-red.svg)](https://GitHub.com/jakobbossek/ecr3vis)

**NOTE:** **Under development** **NOTE**: It is likely that **ecr3vis**
will be merged into **ecr3** in the course of development.

Introduction
------------

**ecr3vis** is the visualization module of **ecr3** (under development).
It offers a collection of functions for the visualization of results of
randomized search heuristics. The focus is on multi-objective problems.
The package includes 2d- and 3d-scatter-plots, parallel coordinate plots
(PCP), heatmaps etc.

Example
-------

In the following we demonstrate how to build a simple mutation-based EA
to optimize the Pseudo-boolean function ONEMAX which counts the number
of ones in a bistring of length n, i.e., the optimum is obviously the
all-ones bit-string. We first define the fitness function that guides
the evolutionary search.

``` r
library(ecr3vis)
library(tidyverse)

# import sample data-set
tbl = readr::read_delim("inst/sampledf.csv", delim = " ")
tbl = filter(tbl, repl == 1L)

plot_scatter2d(tbl, colour = "algorithm")

# Add third objective
tbl$y3 = tbl$y2 + 1
plot_pcp(tbl)
plot_heatmap(tbl)
plot_radar(tbl[1:3, ])
plot_radar(tbl[1:3, ]) + facet_grid(. ~ nr)
```

Development Team
----------------

The package is a one-man project by [Jakob
Bossek](https://researchers.adelaide.edu.au/profile/jakob.bossek) at the
moment of writing. However, the package interfaces some neat
implementations of various other people (see DESCRIPTION file for
details).

How to contribute?
------------------

You can contribute by identifing annoying bugs in the [issue
tracker](http://github.com/jakobbossek/ecr3vis). This is also the
preferred place to ask questions and raise feature requests. Moreover,
users can contribute even more by
[forking](https://help.github.com/en/github/getting-started-with-github/fork-a-repo)
the ecr3vis repository, implementing feautures or bugfixes and raising a
[pull
request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).

Installation Instructions
-------------------------

The package will be available at [CRAN](http://cran.r-project.org) *when
it is done*. If you are interested in trying out and playing around with
the current github developer version use the
[devtools](https://github.com/hadley/devtools) package and type the
following command in R:

``` r
remotes::install_github("jakobbossek/ecr3vis")
```

Getting help
------------

Please address questions and missing features about the *ecr3vis* as
weell as annoying bug reports in the [issue
tracker](https://github.com/jakobbossek/ecr3vis/issues). Pay attention
to explain your problem as good as possible. At its best you provide an
example, so I can reproduce your problem quickly. Please avoid sending
e-mails.
