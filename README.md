# netgen: Generating random (clustered) networks in R

Travis CI build status: [![Build Status](https://travis-ci.org/jakobbossek/netgen.svg?branch=master)](https://travis-ci.org/jakobbossek/netgen)

## Descritption

The *netgen* package offers several methods to generate random graphs respectively
networks for benchmarking purposes. You may want to generate some random graphs
or clustered graphs to perform benchmark studies on some fine algorithms for
the travelling salesperson problem as an example. Examples of generated networks 
are depicted in the following figure.
![Examplary networks](https://raw.githubusercontent.com/jakobbossek/netgen/master/images/instance_examples.png)

## Installation

Currently there is only this developement version of netgen. The package will
be available on CRAN soon. 
To install the current developement version of the package, install the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) by Hadley Wickham, load it and type the following line to a R session:

```splus
install_github("jakobbossek/netgen")
```

## Quickstart

To generate a random network in the euclidean plane [0,1] x [0,1] we simply
load the package and call the `generateRandomInstance` function, providing the
desired number of nodes.

```splus
library(netgen)
set.seed(123)
x = generateRandomInstance(n.points = 50L)
print(x)
autoplot(x)
```

To generate a network with points distributed around several clusters in the
[0,100] x [0, 100] space make use of the more powerful `generateClusteredInstance`
function. It is mandatory to provide the number of points `n.points` and the number of clusters `n.cluster`.

**Note:** See the documentation of the function to read about how it works internally.

```splus
library(netgen)
set.seed(1)
y = generateClusteredInstance(n.cluster = 5L, n.points = 150, upper = 100)
print(x)
autoplot(x)
```


## Contact

Please address questions and missing features about the **netgen package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jbossek/netgen/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.



