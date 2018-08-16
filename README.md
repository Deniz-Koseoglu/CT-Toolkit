# CT-Toolkit
A Classification Tree (CT) and running correlation toolkit created in [RStudio](https://www.rstudio.com/products/rstudio/download/) (version 3.4.2) as a supplement to the following publication: 

D. Köseoğlu, S.T. Belt, K. Husum, and J. Knies (2018), "An assessment of biomarker-based multivariate classification methods versus the PIP<sub>25</sub> index for paleo Arctic sea ice reconstruction" (manuscript submitted to the <i>Organic Geochemistry</i> journal).

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Description
CT-Toolkit contains two functions as of version 1.0.0 (released 12/08/2018).

The `easy_rpart` function is essentially a wrapper that relies on other packages used to build, prune (based on optimisation of performance metrics), test, and visualise the results of Classification Trees constructed via the CART algorithm (Breiman et al., 1984).

The `runcor_analysis` function represents another wrapper used for computing and visualising running/rolling correlations (Pearson, Kendall, or Spearman) between two specified variables using either absolute values, first absolute differences, and/or first relative differences.

# Getting started
1. Install both [R](https://cran.r-project.org/mirrors.html) and [RStudio](https://www.rstudio.com/products/rstudio/download/) using the provided links to official sources.
2. Download the latest CT-Toolkit release from the [repository root](https://github.com/Deniz-Koseoglu/CT-Toolkit) and unpack the .zip archive into an easy-to-find directory of your choice.
3. Open RStudio and create a new R script via **File -> New File -> R Script**.
4. Source the CT Toolkit functions  from the "CT_Toolkit.R" file located in the directory to which the CT-Toolkit .ZIP archive was unpacked. For example, assuming the "CT_Toolkit.R" file is located at C:/CT-Toolkit, the following command may be used:
```r
source("C:/CT-Toolkit/CT_Toolkit.R")
```
5. Note that the "CT-Toolkit" folder contains the "CT_Toolkit.R" file, which can be opened in RStudio to examine and alter the source code, if required.

# Functions
### The `easy_rpart` function
#### Description


#### Usage


#### Arguments


#### Details


#### Values


### The `runcor_analysis` function
#### Description


#### Usage


#### Arguments


#### Details

#### Values

