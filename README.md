#MultiLevelIPF [![Build Status](https://travis-ci.org/krlmlr/MultiLevelIPF.png)](https://travis-ci.org/krlmlr/MultiLevelIPF)

Implementation of algorithms that extend IPF to nested structures.

The IPF algorithm operates on count data.  This package offers implementations for several algorithms that extend this to nested structures: "parent" and "child" items for both of which constraints can be provided.

Install using

```s
library(devtools)
install_github("krlmlr/MultiLevelIPF")
```

(requires `devtools` 1.4 or later) or

```s
library(devtools)
install_github("MultiLevelIPF", "krlmlr")
```

My [`kimisc` package](https://github.com/krlmlr/kimisc) is also required.
