# Phylin

*Phylin* is an R package for the spatial interpolation of genetic distances. The inteprolation between samples is based on a modified kriging method that accepts a genetic distance matrix and generates a map of probability of lineage presence. This package also offers tools to generate a map of potential contact zones between groups with user-defined thresholds in the tree to account for old and recent divergence. Additionally, it has functions for IDW interpolation using genetic data and midpoints.

## Installation

There are several option to install *phylin*:

- Install the version at CRAN as usual. This may not be the latest, latest version but it already passed all CRAN checks.

- To install the current development version found here, you need to download the built *tar.gz* package file and install it. Windows users may have to use [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to rebuild the package.

- Clone the repository with *git* and build the package in your computer. 

- Use the *devtools* package in R. Example code:

```
    library(devtools)
    install_github("ptarroso/phylin", subdir="source")
```

## Using phylin

*Phylin* has an example dataset and functions are documented and with example code. You can also check the online [tutorial](http://webpages.icav.up.pt/pessoas/ptarroso/phylin/phylin.html) or the vignette also included in the package with:

```
vignette("phylin_tutorial")
```


