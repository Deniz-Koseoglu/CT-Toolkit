# Using CT Toolkit
This vignette was created to briefly showcase the usage of CT Toolkit functions, including `easy_rpart` and `runcor_analysis` (as of version 1.0.0). For more detailed installation and usage instructions, please refer to the [full manual](https://github.com/Deniz-Koseoglu/CT-Toolkit/blob/master/README.md).

Please note that the vignette assumes basic knowledge of R syntax (version >=3.4.2).

# Setting up a working directory
Following the installation of CT Toolkit (see [Getting Started](https://github.com/Deniz-Koseoglu/CT-Toolkit#getting-started)), it might be useful to create a centralised folder to keep all input datasets, as well as exported results.

The example structure used here is located on **Drive C:/CT Toolkit/** as follows:
![Image1](https://image.ibb.co/dMh7Ne/Untitled.png)

This allows for easy storage of **training datasets**, **prediction datasets**, and **CT Toolkit output** in the Training, Prediction, and Results folders, respectively.
Once the folder structure has been created, the CT_Toolkit.R file, which contains the source code, may be sourced from R (or RStudio) as follows:
```r
source(C:/CT Toolkit/CT_Toolkit.R)
```
