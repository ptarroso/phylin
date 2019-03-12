# Version 2.0
* User defines the distance metric to be used in the interpolation process
* Genetic distance uncertainty can be used to build the variogram
* Overall performance improvement
* New vignette describing new features in this version

# Version 1.2.2
* added functions 'print.gv' and 'summary.gv' for fast view of gv object data

# Version 1.2.1
* Improved variogram model fitting in function gv.model (and related functions mtest.gv and predict.gv)
* Added new models (pentaspherical and cubic)
* Improved the calculation of the linear model with sill
* Improved the nls fitting

# Version 1.2.0

* Improve distance calculations with resistance/friction information using 'gdistance' package.
* The "krig" function was modified to accept a distance function to calculate distances. The 'default' uses simple euclidean distances between coordinates to maintain compatibility with code for previous versions.
* Plot method for 'gv' was duplicated and is now corrected.
* Plotting gv with multiple trees now displays CI bars instead of lines.
* Bug corrected: plot.gv wasn't displaying the variogram correctly if NA were present.

# Version 1.1.1

* Explicit declaration of imports in NAMESPACE to avoid a NOTE during CRAN check.

# Version 1.1.0

## Major changes

* Variograms can now be generated with the full (or part of) tree posterior probability instead of a single consensus tree.
* Added a generic function to generate the variogram if a single or multiple trees are found.
* Added new variogram plot to deal with multiple trees.

## Minor changes

* If the variogram produces NAs, a warning is thrown.
* Krig function does a check to verify if grid coordinates are given as a 'data.frame'.


# Version 1.0.1

* Some minor fixes to correct the NOTES on CRAN checks (register S3 classes on NAMESPACE)
* The tutorial as added as a vignette.


# Version 1.0

* First version submitted to CRAN
