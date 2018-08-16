# Using CT Toolkit
This vignette was created to briefly showcase the usage of CT Toolkit functions, including `easy_rpart` and `runcor_analysis` (as of version 1.0.0). For more detailed installation and usage instructions, please refer to the [full manual](https://github.com/Deniz-Koseoglu/CT-Toolkit/blob/master/README.md).

Please note that the vignette assumes basic knowledge of R syntax (version >=3.4.2).

# Setting up a working directory
Following the installation of CT Toolkit (see [Getting Started](https://github.com/Deniz-Koseoglu/CT-Toolkit#getting-started)), it might be useful to create a centralised folder to keep all input datasets, as well as exported results.

The example structure used here is located on **Drive C:/CT Toolkit/** as follows:
![Image1](https://image.ibb.co/dMh7Ne/Untitled.png)

This allows for easy storage of **training datasets**, **prediction datasets**, and **CT Toolkit output** in the Training, Prediction, and Results folders, respectively.
Once the folder structure has been created, the **CT_Toolkit.R** file (available [here]()), which contains the source code, may be sourced from R (or RStudio) as follows:
```r
source("C:/CT Toolkit/CT_Toolkit.R")
```

# Using the functions
#### Downloading example data
1. The example training dataset to build Classification Tree (CT) models, which contains multivariate Highly-Branched Isoprenoid (HBI) data for 198 surface sediments in the Barents Sea, may be downloaded [here](). Place this file in the **C:/CT Toolkit/Training** folder.

2. The example prediction datasets, which contain HBI data for several marine sediment cores, may be downloaded [here](). Place the 3 files in the **C:/CT Toolkit/Prediction** folder. We will reconstruct sea ice conditions for these sedimentary records via the CT model trained using surface sediments.

#### Downloading example code
All code showcasing the functions in this vignette may be downloaded [here]().

#### Creating CT models
Using the example data that we just downloaded in conjunction with the `easy_rpart` function, we can create and test a CT model via the following code:
```r
easy_rpart_results <- easy_rpart(training_data = "C:/CT Toolkit/Training/BS_surface_seds.csv",
                                 separ=",",
                                 target_var = list("Group", c("Mar", "Int", "Ext")),
                                 descriptive_vars = c("I", "II", "III", "IV"),
                                 scale_vars=FALSE,
                                 descvar_units = "%",
                                 split_criterion = "information",
                                 class_method = "CART",
                                 prune_method = "both",
                                 tree_number = 300,
                                 rf_vars = "auto",
                                 cv_method = "repeatedcv",
                                 folds=10,
                                 replicates=5,
                                 train_percent=70,
                                 tuning_metric="Kappa",
                                 predictions=TRUE,
                                 prediction_datasets="C:/CT Toolkit/Prediction",
                                 extension="\\.csv$",
                                 plot_predictions = TRUE,
                                 prediction_axis = c("Age", "SpSIC"),
                                 x_lab="Age (cal kyr BP)",
                                 y_lab=expression(P[III]*IP[25]*"-derived SpSIC (%)"),
                                 y_limit=c(-20, 100),
                                 plot_cols=c("red", "orange", "green"),
                                 export_results=TRUE,
                                 export_plots=TRUE,
                                 width=10,
                                 height=10,
                                 export_path="C:/CT Toolkit/Results",
                                 seed_val=42)
```
Each argument is presented on a new line for clarity. Try changing some of the values for the function arguments (the full list and descriptions of which are available [here]()), and re-run the code.

#### Calculating running correlations
For running correlations, we will use the following code from the `runcor_analysis` function to calculate correlation between IP<sub>25</sub> and IPSO<sub>25</sub> in sediment core KA11:
```r
runcor_results <- runcor_analysis(data_path="C:/CT Toolkit/Prediction/KA11.csv",
                                  separ=",",
                                  cor_vars=c("IP25", "IPSO25"),
                                  runcor_permutations="all",
                                  cor_method="pearson",
                                  confid_lvl=0.95,
                                  cor_window=9,
                                  move_by=1,
                                  cor_align="center", # Currently the only option implemented!
                                  output_vars=c("Age", "HBI_III", "HBI_IV", "SpSIC"),
                                  x_var="Age",
                                  x_lab="Age (cal kyr BP)",
                                  y_limit=c(-1, 1), #"none"
                                  pval_corrections = "bonferroni",
                                  pval_labels = "auto",
                                  plot_cols=c("red", "orange", "darkgreen", "black"),
                                  chgpt_methods = "CP3O",
                                  export_results=TRUE,
                                  export_path="C:/CT Toolkit/Results",
                                  print_plots=TRUE,
                                  export_plots=TRUE,
                                  width=10,
                                  height=7)

```
The full list and descriptions of `runcor_analysis` arguments are available [here]()).
