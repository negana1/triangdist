# triangdist

The `triangdist` package provides functions for the **Triangular Distribution** in R,
providing the standard four functions: density, distribution function, quantile function, and random generation.
The package follows standard R conventions and supports vectorized inputs.

## Installation

You can install the development version of `triangdist` from GitHub with:
```r
# install.packages("remotes")
remotes::install_github("negana1/triangdist")
```

## Example
```r
library(triangdist)

# Density at x = 0.5 with min=0, max=1, mode=0.5
dtriang(0.5, min = 0, max = 1, mode = 0.5)

# Cumulative probability
ptriang(0.5, min = 0, max = 1, mode = 0.5)

# Quantile
qtriang(0.5, min = 0, max = 1, mode = 0.5)

# Random sample
set.seed(42)
rtriang(10, min = 0, max = 1, mode = 0.5)
```
