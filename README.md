# MultiLevelIPF [![wercker status](https://app.wercker.com/status/4c6df82f657e4fee539c60601da8944d/s/master "wercker status")](https://app.wercker.com/project/bykey/4c6df82f657e4fee539c60601da8944d) [![codecov.io](https://codecov.io/github/krlmlr/MultiLevelIPF/coverage.svg?branch=master)](https://codecov.io/github/krlmlr/MultiLevelIPF?branch=master)

Implementation of algorithms that extend IPF to nested structures.

The IPF algorithm operates on count data.  This package offers implementations for several algorithms that extend this to nested structures: "parent" and "child" items for both of which constraints can be provided.


## Powered by

- [`grake`](http://krlmlr.github.io/grake): A reimplementation of generalized raking ([Deville and Särndal, 1992](http://amstat.tandfonline.com/doi/abs/10.1080/01621459.1992.10475217); [Deville, Särndal and Sautory, 1993](http://www.tandfonline.com/doi/abs/10.1080/01621459.1993.10476369))


## Related work

- [`wrswoR`](http://krlmlr.github.io/wrswoR): An implementation of fast weighted random sampling without replacement ([Efraimidis and Spirakis, 2006](http://www.sciencedirect.com/science/article/pii/S002001900500298X))

- [`mangow`](http://krlmlr.github.io/mangow): Embed the Gower distance metric in L1

- [`RANN.L1`](https://github.com/jefferis/RANN/tree/master-L1#readme): k-nearest neighbors using the L1 metric


## Installation

```s
devtools::install_github("krlmlr/MultiLevelIPF")
```
