# Version 1.2.0

* Improve distance calculations with resistance/friction information using 'gdistance' package.	

	
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
	
