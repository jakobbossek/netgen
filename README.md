# netgen: Generating random (clustered) networks in R

[![CRAN](https://img.shields.io/badge/cran-1.1-green.svg)](http://cran.r-project.org/web/packages/netgen/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/netgen.svg?branch=master)](https://travis-ci.org/jakobbossek/netgen)
[![Build status](https://ci.appveyor.com/api/projects/status/gpbans0vvbfcwyty/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/netgen/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/netgen/badge.svg)](https://coveralls.io/r/jakobbossek/netgen)

## Descritption

The *netgen* package offers several methods to generate random graphs respectively
networks for benchmarking purposes. You may want to generate some random graphs
or clustered graphs to perform benchmark studies on some fine algorithms for
the travelling salesperson problem as an example. Examples of generated networks 
are depicted in the following figure.
![Examplary networks](https://raw.githubusercontent.com/jakobbossek/netgen/master/images/instance_examples.png)

## Installation

The package has recently been submitted to CRAN. Find additional information on the [official CRAN website](http://cran.r-project.org/web/packages/netgen/).
To install the current developement version of the package, install the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) by Hadley Wickham, load it and type the following line to a R session:

```splus
install_github("jakobbossek/netgen")
```

## Quickstart

To generate a random network in the euclidean plane [0,1] x [0,1] we simply
load the package and call the `generateRandomNetwork` function, providing the
desired number of nodes.

```splus
library(netgen)
set.seed(123)
x = generateRandomNetwork(n.points = 50L)
print(x)
autoplot(x)
```

To generate a network with points distributed around several clusters in the
[0,100] x [0, 100] space make use of the more powerful `generateClusteredNetwork`
function. It is mandatory to provide the number of points `n.points` and the number of clusters `n.cluster`.

**Note:** See the documentation of the function to read about how it works internally.

```splus
library(netgen)
set.seed(1)
x = generateClusteredNetwork(n.cluster = 5L, n.points = 150L, upper = 100)
print(x)
autoplot(x)
```


## Contact

Please address questions and missing features about the **netgen package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jbossek/netgen/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.



