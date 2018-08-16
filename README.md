# CT-Toolkit
A Classification Tree (CT) and running correlation toolkit created in [RStudio](https://www.rstudio.com/products/rstudio/download/) (version 3.4.2) as a supplement to the following publication: 

D. Köseoğlu, S.T. Belt, K. Husum, and J. Knies (2018), "An assessment of biomarker-based multivariate classification methods versus the PIP<sub>25</sub> index for paleo Arctic sea ice reconstruction" (manuscript submitted to the <i>Organic Geochemistry</i> journal).

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Description
CT-Toolkit contains two functions as of version 1.0.0 (released 12/08/2018).

The `easy_rpart` function is essentially a wrapper that relies on other packages used to build, prune (based on optimisation of performance metrics), test, and visualise the results of Classification Trees constructed via the CART algorithm (Breiman et al., 1984).

The `runcor_analysis` function represents another wrapper used for computing and visualising running/rolling correlations (Pearson, Kendall, or Spearman) between two specified variables using either absolute values, first absolute differences, and/or first relative differences.

# Dependencies
R (>=3.4.2), rpart, data.table, car, readr, rattle, rpart.plot, e1071, DMwR, MLmetrics, rpartScore, pROC, caret, xlsx, randomForest, qpcR, scales, ggplot2, RColorBrewer, gtools, ecp, changepoint.

# Getting started
1. Install both [R](https://cran.r-project.org/mirrors.html) and [RStudio](https://www.rstudio.com/products/rstudio/download/) using the provided links to official sources.
2. Download the latest CT-Toolkit release from the [repository root](https://github.com/Deniz-Koseoglu/CT-Toolkit) and unpack the .zip archive into an easy-to-find directory of your choice.
3. Open RStudio and create a new R script via **File -> New File -> R Script**.
4. Source the CT Toolkit functions  from the "CT_Toolkit.R" file located in the directory to which the CT-Toolkit .ZIP archive was unpacked. For example, assuming the "CT_Toolkit.R" file is located at C:/CT-Toolkit, the following command may be used:
```r
source("C:/CT-Toolkit/CT_Toolkit.R")
```
You are now ready to use the functions.
5. Note that the "CT-Toolkit" folder contains the "CT_Toolkit.R" file, which can be opened in RStudio to examine and alter the source code, if required.

# Functions
### The `easy_rpart` function
#### Description
A function to build, prune, and test Classification Tree and Random Forest models. In essence, this is a wrapper for the CART-based package "rpart" and the "randomForest" package.

#### Usage
```r
easy_rpart(training_data = paste(getwd(), "/Training", sep=""), separ = ",", target_var, descriptive_vars, scale_vars=FALSE, 
descvar_units="%", split_criterion = "information", class_method = "CART", prune_method = "both", tree_number = 500, rf_vars = 3, 
cv_method = "repeatedcv", folds = 10, replicates = 5, train_percent = 70, tuning_metric = "Kappa", predictions = TRUE, 
prediction_datasets = paste(getwd(), "/Prediction",sep=""), extension = "\\.csv$", plot_predictions = TRUE, 
prediction_axis = c("Age","SpSIC"), plot_grp_labs=waiver(), tvar_lines=c(10,50), plot_cols=brewer.pal(5, "Set3"), export_results = TRUE, 
export_plots=TRUE, x_lab=waiver(), y_lab=waiver(), y_limit = NULL, y_cut = waiver(), width = 10, height = 10, export_path = getwd(), 
seed_val = 42)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **training_data** | A character value with the full path to the training dataset in .CSV format. If no value is explicitly provided, defaults to the working directory, which should contain a folder labelled "Training" containing the training dataset. |
| **separ** | Separator value to use for imported .CSV files. Defaults to a comma.|
| **target_var** | A character value with the column name containing class information for the training dataset. Can also be provided as a list of 2 or 3 elements. If a list of 2 is provided, the first element should be the column name containing the class information, while the second element should contain the preferred order of the classes that will be shown in graphical output. A list of 3 should only be provided if the class information is not readily available and should instead be derived using threshold values of a data column (e.g. using 10% and 50% cutoff values of spring sea ice concentration to create 3 classes of sea ice conditions). In such cases, the first list element should contain the column name from which to derive class data, while the second element should contain the threshold values to derive class information from and the desired class labels, respectively.|
| **descriptive_vars** | A character vector of column names from the training dataset to be used as descriptive variables for model training. |
| **scale_vars** | A TRUE/FALSE logical that scales the descriptive variables defined by the **descriptive_vars** argument to percentages of their total. Defaults to FALSE, thus assuming that the descriptive variables have already been scaled. |
| **descvar_units** | A character value denoting descriptive variable units to show in graphical output, such as the hierarchical CT structure. Defaults to "%". |
| **split_criterion** | The splitting criterion to use for determining node impurity when calculating splitting rules. The default is "information", which uses Information Gain. The Gini Index can also be used by assigning the value "gini". This argument is internally passed to the **rpart** function from the eponymous package. |
| **class_method** | The type of classification model to build, passed as a character value. Can be one of: "CART" or "RF". Defaults to "CART", while Random Forest models can be built using the alternative "RF". |
| **prune_method** | The pruning strategy to use for determination of optimal tree size. Can be one of: "MinCP", "1SE" or "both" (the default), corresponding to using the minimum cross-validation error, 1-Standard Error rule, or both methods, respectively.|
| **tree_number** | Only relevant when **class_method** is "RF". The number of trees to include in the random forest model (defaults to 500). |
| **rf_vars** | Only relevant when **class_method** is "RF". The number of descriptive variables to try at each split when building a random forest model (defaults to 3). |
| **cv_method** | The cross validation method to use for reducing positive bias when calculating model performance metrics. Can be one of: "repeatedcv" (the default), "LOOCV", "LGOCV", or "oob". The out-of-bag error estimate is only available when **class_method** is "RF". |
| **folds** | The number of folds to use for cross-validation. Defaults to 10. |
| **repeats** | The number of times to repeat the cross-validation procedure before final performance metrics are aggregated. Defaults to 5. |
| **train_percent** | Only used when **cv_method** is "LGOCV". The percentage of the training set to use for model training. The remainder will be used for testing. Defaults to 70. |
| **tuning_metric** | The performance metric to maximize during tree pruning to select the optimal model. Can be one of: "Accuracy" or "Kappa" (the default). |
| **predictions** | A TRUE/FALSE logical. Should the trained CT or RF model be used to classify new samples (e.g. downcore records)? Defaults to TRUE. |
| **prediction_datasets** | The directory containing dataset(s) to derive class predictions for, passed as a character value. If no value is explicitly provided, defaults to the working directory, which should contain a folder labelled "Prediction" containing the prediction dataset(s). |
| **extension** | The file extension of the prediction datasets. **NOTE**: Currently, only detecting .CSV files is supported. |
| **plot_predictions** | A TRUE/FALSE logical. Should the model-derived classes for the prediction dataset(s) be plotted? Defaults to TRUE. |
| **prediction_axis** | A character vector of length 2, where the first and second elements are column names which should be used for the x and y axis of the prediction dataset plots. Defaults to c("Age", "SpSIC"), which assumes these columns are present in all prediction datasets. |
| **plot_grp_labs** | The labels to use for predicted classes in the prediction dataset plots. By default, these are automatically taken from the training data. |
| **tvar_lines** | Draws dashed horizontal lines at specified y-values in the prediction dataset plots (default values are 10 and 50). Useful for displaying class boundaries used in the training set. |
| **plot_cols** | A character vector of colours to use for different classes in the prediction dataset plots. By default, these are generated automatically.  |
| **export_results** | A TRUE/FALSE logical. Should model performance metrics and class predictions be exported as an .XLSX file? Defaults to TRUE. |
| **export_plots** | A TRUE/FALSE logical. Should plots be exported as a .PDF file? Defaults to TRUE. |
| **x_lab** | The x-axis label to use for prediction dataset plots. Determined automatically from the data by default. |
| **y_lab** | The y-axis label to use for prediction dataset plots. Determined automatically from the data by default. |
| **y_limit** | Defaults to NULL. |
| **y_cut** |  |
| **width** | The width of exported .PDF plots. Defaults to 10 inches. |
| **height** | The height of exported .PDF plots. Defaults to 10 inches. |
| **export_path** | The character value for a directory to export results and plots to. Defaults to the working directory. |
| **seed_val** | The random seed value to use for reproducible results with a given dataset. Can be any integer. The default is 42. |

#### Details


#### Values


### The `runcor_analysis` function
#### Description


#### Usage


#### Arguments


#### Details

#### Values

