# CT-Toolkit
A Classification Tree (CT) and running correlation toolkit created in R Studio (version 3.4.2) as a supplement to the following publication: 

D. Köseoğlu, S.T. Belt, K. Husum, and J. Knies (2018), "An assessment of biomarker-based multivariate classification methods versus the PIP<sub>25</sub> index for paleo Arctic sea ice reconstruction" (manuscript submitted to the <i>Organic Geochemistry</i> journal).

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Description
CT-Toolkit contains two functions as of version 1.0.0 (released 12/08/2018).

The `easy_rpart` function is essentially a wrapper that relies on other packages used to build, prune (based on optimisation of performance metrics), test, and visualise the results of Classification Trees constructed via the CART algorithm (Breiman et al., 1984).

The `runcor_analysis` function represents a second wrapper used for computing and visualising running/rolling correlations between two specified variables using either absolute values, first absolute differences, and/or first relative differences.

# Functions
## The `easy_rpart` function

## The `runcor_analysis` function
