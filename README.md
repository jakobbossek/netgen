
# netgen: Generating random (clustered) networks in R

<!-- badges: start -->

[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/netgen)](http://cran.r-project.org/web/packages/netgen)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/netgen)](http://cran.rstudio.com/web/packages/netgen/index.html)
[![R-CMD-check](https://github.com/jakobbossek/netgen/workflows/R-CMD-check/badge.svg)](https://github.com/jakobbossek/netgen/actions)
[![Codecov test
coverage](https://codecov.io/gh/jakobbossek/netgen/branch/master/graph/badge.svg)](https://codecov.io/gh/jakobbossek/netgen?branch=master)
<!-- badges: end -->

## Description

The *netgen* package offers several methods to generate random graphs
respectively networks for benchmarking purposes. You may want to
generate some random graphs or clustered graphs to perform benchmark
studies on some fine algorithms for the travelling salesperson problem
as an example. Examples of generated networks are depicted in the
following figure. ![Examplary
networks](https://raw.githubusercontent.com/jakobbossek/netgen/master/images/instance_examples.png)

## Installation

The package has recently been submitted to CRAN. Find additional
information on the [official CRAN
website](http://cran.r-project.org/web/packages/netgen/). To install the
current developement version of the package, install the [devtools
package](http://cran.r-project.org/web/packages/devtools/index.html) by
Hadley Wickham, load it and type the following line to a R session:

``` splus
install_github("jakobbossek/netgen")
```

## Quickstart

To generate a random network in the euclidean plane \[0,1\] x \[0,1\] we
simply load the package and call the `generateRandomNetwork` function,
providing the desired number of nodes.

``` splus
library(netgen)
set.seed(123)
x = generateRandomNetwork(n.points = 50L)
print(x)
autoplot(x)
```

To generate a network with points distributed around several clusters in
the \[0,100\] x \[0, 100\] space make use of the more powerful
`generateClusteredNetwork` function. It is mandatory to provide the
number of points `n.points` and the number of clusters `n.cluster`.

**Note:** See the documentation of the function to read about how it
works internally.

``` splus
library(netgen)
set.seed(1)
x = generateClusteredNetwork(n.cluster = 5L, n.points = 150L, upper = 100)
print(x)
autoplot(x)
```

## News

### netgen v1.3 (Release data: 2016-01-16):

-   Default number of digits for exportToTSPlibFormat is 10 now
-   Added function isEuclidean
-   rescaleNetwork now supports instances with depots
-   rescaleNetwork now recomputes distance matrix
-   rescaleNetwork throws error on non-euclidean networks
-   Added function getTSPInstancesOverview to get an overview of files
    in TSPlib format
-   Added function filterTSPInstances which allows to subset/filter TSP
    instances in TSPlib format by properties.
-   Networks have an additional property edge.weight.type now (see
    TSPlib documentation for details)
-   Added function getValidEdgeWeightTypes

### netgen v1.2 (Release data: 2015-11-24):

-   last line of TSPlib export now containts EOF string
-   function importFromTSPlibFormat now has an additional argument
    round.distances with default value TRUE which decides whether
    distances of EUC_2D instances should be rounded to the nearest
    integer value

### netgen v1.1 (Release data: 2015-05-19):

-   added rescaleNetwork
-   network objects now store the distance matrix internally
-   network objects now have name and comment field
-   lower and upper bound are now optional for
    makeNetwork/makeClusteredNetwork
-   added possibility to pass point matching or point matching algorithm
    to morphInstances
-   getOptimalPointAssignment now has another parameter method (new
    method to solve the assigment to optimality)
-   importFromTSPlibFormat and exportToTSPlibFormat for storing networks
    in the TSPlib format
-   importFromFile and exportToFile for storing networks in proprietary
    format (similar to TSPlib format, but with additional information
    like cluster membership or arrival/request times)
-   generate{Random,Clustered,Grid}Network now automatically generate a
    random name if none is provided
-   there is no makeClusteredNetwork function anymore. Instead
    makeNetwork has an optional paramter membership

### netgen v1.0 (Release date: 2015-02-03):

-   First submission to CRAN.

## Contact

Please address questions and missing features about the **netgen
package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some
nasty bugs? Please use the [issue
tracker](https://github.com/jbossek/netgen/issues) for this. Pay
attention to explain the problem as good as possible. At its best you
provide an example, so I can reproduce your problem.
