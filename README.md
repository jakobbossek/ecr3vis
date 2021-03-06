
# ecr3vis: ecr3 visualization

**Visit the [package website](https://jakobbossek.github.io/ecr3vis/).**

**NOTE:** **Under heavy development!!! :construction:**  
It is likely that **ecr3vis** will be merged/renamed into **ecr3** in
the course of development.

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Status](https://img.shields.io/badge/Status-experimental-red.svg)](https://GitHub.com/jakobbossek/ecr3vis)
[![R-CMD-check](https://github.com/jakobbossek/ecr3vis/workflows/R-CMD-check/badge.svg)](https://github.com/jakobbossek/ecr3vis/actions)
[![codecov](https://codecov.io/gh/jakobbossek/ecr3vis/branch/main/graph/badge.svg?token=CJH8ZATFJW)](https://codecov.io/gh/jakobbossek/ecr3vis)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ecr3vis)](https://cran.r-project.org/package=ecr3vis)
<!-- badges: end -->

## Introduction

**ecr3vis** is the visualization module of **ecr3** (under development).
It offers a collection of functions for the visualization of results of
randomized search heuristics. The focus is on multi-objective problems.
The package includes 2d- and 3d-scatter-plots, parallel coordinate plots
(PCP), heatmaps etc.

## Example

In the following we demonstrate how to build a simple mutation-based EA
to optimize the Pseudo-boolean function ONEMAX which counts the number
of ones in a bistring of length n, i.e., the optimum is obviously the
all-ones bit-string. We first define the fitness function that guides
the evolutionary search.

``` r
library(ecr3vis)
library(tidyverse)

# import sample data-set
data(emoas_on_zdt)
tbl = filter(emoas_on_zdt, repl == 1L)

plot_scatter2d(tbl, colour = "algorithm")

# Add third objective
tbl$y3 = tbl$y2 + 1
plot_heatmap(tbl)
plot_radar(tbl[1:3, ])
plot_radar(tbl[1:3, ]) + facet_grid(. ~ nr)
```

## Development Team

The package is a one-man project by [Jakob
Bossek](https://researchers.adelaide.edu.au/profile/jakob.bossek) at the
moment of writing. However, the package interfaces some neat
implementations of various other people (see DESCRIPTION file for
details).

## How to contribute?

You can contribute by identifing annoying bugs in the [issue
tracker](http://github.com/jakobbossek/ecr3vis). This is also the
preferred place to ask questions and raise feature requests. Moreover,
users can contribute even more by
[forking](https://help.github.com/en/github/getting-started-with-github/fork-a-repo)
the ecr3vis repository, implementing feautures or bugfixes and raising a
[pull
request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).

## Installation Instructions

The package will be available at [CRAN](http://cran.r-project.org) *when
it is done*. If you are interested in trying out and playing around with
the current github developer version use the
[devtools](https://github.com/hadley/devtools) package and type the
following command in R:

``` r
remotes::install_github("jakobbossek/ecr3vis")
```

## Getting help

Please address questions and missing features about the *ecr3vis* as
weell as annoying bug reports in the [issue
tracker](https://github.com/jakobbossek/ecr3vis/issues). Pay attention
to explain your problem as good as possible. At its best you provide an
example, so I can reproduce your problem quickly. Please avoid sending
e-mails.

## Related Software

There are many alternatives in both R and other languages. We list some
examples in the following:

-   [**ecr**](https://cran.r-project.org/package=ecr) *Evolutionary
    Computation in R*, short **ecr**, is the predecessor of **ecrvis**.
-   [**eaf**](https://cran.r-project.org/package=eaf) R package with
    functions to plot *Empirical Attainment Functions* (EAF) and
    EAF-differences. In addition, the package offers various
    multi-objective performance indicators.
-   [**mco**](https://cran.r-project.org/package=mco) A R package with
    focus on the rapid implementation of multi-objective evolutionary
    algorithms only.
-   [**emoa**](https://cran.r-project.org/package=emoa) Another R
    package with focus on the development of multi-objective
    evolutionary algorithms (same author as in **mco**).
-   [**GA**](https://cran.r-project.org/package=GA) R package with some
    building blocks for evoltionary algorithms. Also offers the
    possibility for island model EAs. However, restricted to standard
    representations and single-objective problems.
-   [**MaOEA**](https://cran.r-project.org/package=MaOEA): A set of
    evolutionary algorithms to solve many-objective optimization
    (i.e. problems with more than three objectives).
-   [**pymoo**](https://pymoo.org) A quite new, yet comprehensive,
    Python library for evolutionary multi-objective optimization.
-   [**DEAP**](https://github.com/deap/deap) Another prominent and
    established Python framework on evolutionary computation with a wide
    range of functionality.
