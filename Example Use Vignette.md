# Using CT Toolkit
This vignette was created to showcase the usage of CT Toolkit functions, including `easy_rpart` and `runcor_analysis` (as of version 1.0.0). For more detailed installation and usage instructions, please refer to the [full manual](https://github.com/Deniz-Koseoglu/CT-Toolkit/blob/master/README.md).

Please note that the vignette assumes basic knowledge of R syntax (version >=3.4.2).

The current version of CT Toolkit (1.0.0) has so far been tested solely on the Windows 10 operating system.

# Setting up a working directory
Following the installation of CT Toolkit (see [Getting Started](https://github.com/Deniz-Koseoglu/CT-Toolkit#getting-started)), it might be useful to create a centralised folder to keep all input datasets, as well as exported results.

The example structure used here is located on **Drive C:/CT Toolkit/** as follows:
![Image1](https://image.ibb.co/dMh7Ne/Untitled.png)

This allows for easy storage of **training datasets**, **prediction datasets**, and **CT Toolkit output** in the Training, Prediction, and Results folders, respectively.
Once the folder structure has been created, the **CT_Toolkit.R** file (available [here](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/R%20Files)), which contains the source code, may be sourced from R (or RStudio) as follows:
```r
source("C:/CT Toolkit/CT_Toolkit.R")
```

# Using the functions
#### Downloading example data
1. The example training dataset to build Classification Tree (CT) models, which contains multivariate Highly-Branched Isoprenoid (HBI) data for 198 surface sediments in the Barents Sea, may be downloaded [here](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/Example%20Data/Training). Place this file in the **C:/CT Toolkit/Training** folder.

2. The example prediction datasets, which contain HBI data for several marine sediment cores, may be downloaded [here](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/Example%20Data/Prediction). Place the 3 files in the **C:/CT Toolkit/Prediction** folder. We will reconstruct sea ice conditions for these sedimentary records via the CT model trained using surface sediments.

#### Downloading example code
All code showcasing the functions in this vignette may be downloaded [here](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/R%20Files).

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
                                 seed_val=90)
```
The **Results** of `easy_rpart` analysis, assuming the function ran correctly, will be exported to **C:/CT Toolkit/Results** and should consist of three files:
![Image2](https://image.ibb.co/f85N5z/Untitled2.png)

1. The file beginning with **"CT_Plots"** contains unpruned (i.e. full tree) and pruned (with both the minimum cross-validation error and 1-SE rule methods) classification tree structures, and associated variable importance plots.

2. The file beginning with **"CT_NewPredictions"** contains plots of downcore predictions of sea ice conditions (in this case) derived from the pruned tree model which yielded the best performance.

3. Finally, the **.xlsx file** contains the pruned model performance metrics and variable importance values (sheets 1 and 2), results of cross validation classification for the training set (sheet 3), and downcore predictions using the pruned trees (sheet 4).

You may want to try changing some of the values for the function arguments (the full list and descriptions of which are available [here](https://github.com/Deniz-Koseoglu/CT-Toolkit#arguments)), and re-run the code to test combinations of parameters and how these affect the output.

#### Calculating running correlations
For running correlations, we will use the following code from the `runcor_analysis` function to correlate IP<sub>25</sub> and IPSO<sub>25</sub> in marine sediment core KA11:
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
                                  y_limit=c(-1, 1),
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
The output of `runcor_analysis` should consist of two files:
![Image3](https://image.ibb.co/i05rXe/Untitled3.png)

1. The **.pdf file** contains the plots for every combination of running correlations, p-value corrections, and changepoint analysis algorithms chosen by the user.

2. The **.xlsx file** contains correlation coefficients and p-values for every data type chosen for the running correlations (in this case: Raw Data, Absolute Differences, and Relative Differences on sheets 1, 2, and 3). The final sheet contains the changepoint locations for each of these data types along the x-axis (which is represented by Age, in cal kyr BP, in this example).

The full list and descriptions of `runcor_analysis` arguments are available [here](https://github.com/Deniz-Koseoglu/CT-Toolkit#arguments-1).
