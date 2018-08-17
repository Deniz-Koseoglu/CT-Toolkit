# CT Toolkit
A Classification Tree (CT) and running correlation toolkit created in [RStudio](https://www.rstudio.com/products/rstudio/download/) (R version 3.4.2) as a supplement to the following publication: 

D. Köseoğlu, S.T. Belt, K. Husum, and J. Knies (2018), "An assessment of biomarker-based multivariate classification methods versus the PIP<sub>25</sub> index for paleo Arctic sea ice reconstruction" (manuscript in review for the <i>Organic Geochemistry</i> journal).

For a simplified introduction to decision tree methodology, training and use of CT models for reconstructing centennial-scale sea ice conditions, please also see:

D. Köseoğlu, S.T. Belt, L. Smik, H. Yao, G. Panieri, and J. Knies (2018), "Complementary biomarker-based methods for characterising Arctic sea ice conditions: A case study comparison between multivariate analysis and the PIP<sub>25</sub> index", *Geochimica et Cosmochimica Acta* **222**, pp. 406-420. DOI: https://doi.org/10.1016/j.gca.2017.11.001. 

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Description
CT-Toolkit contains two functions as of version 1.0.0 (released 16/08/2018).

The `easy_rpart` function is essentially a wrapper that relies on other packages used to build, prune (based on optimisation of performance metrics), test, and visualise the results of Classification Trees constructed via the CART algorithm (Breiman et al., 1984).

The `runcor_analysis` function represents another wrapper used for computing and visualising running/rolling correlations (Pearson, Kendall, or Spearman) between two specified variables using either absolute values, first absolute differences, and/or first relative differences.

# Dependencies
R (>=3.4.2), rpart, data.table, car, readr, rattle, rpart.plot, e1071, DMwR, MLmetrics, rpartScore, pROC, caret, xlsx, randomForest, qpcR, scales, ggplot2, RColorBrewer, gtools, ecp, changepoint.

# Getting started
1. Install both [R](https://cran.r-project.org/mirrors.html) and [RStudio](https://www.rstudio.com/products/rstudio/download/) using the provided links to official sources.
2. Download the latest CT-Toolkit release from the [repository](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/R%20Files) and unpack the .zip archive into an easy-to-find directory of your choice.
3. Open RStudio and create a new R script via **File -> New File -> R Script**.
4. Source the CT Toolkit functions  from the "CT_Toolkit.R" file located in the directory to which the CT-Toolkit .ZIP archive was unpacked. For example, assuming the "CT_Toolkit.R" file is located at C:/CT-Toolkit, the following command may be used:
```r
source("C:/CT-Toolkit/CT_Toolkit.R")
```
You are now ready to use the functions.

5. Note that although the "CT_Toolkit.R" file does not have to be opened directly in R (or RStudio), it contains the source code which can be examined and altered by the user to add or change functionality, if required.

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
| **target_var** | A character value with the column name containing class information for the training dataset. Can also be provided as a list of 2 or 3 elements. If a list of 2 is provided, the first element should be the column name containing the class information, while the second element should contain the preferred order of the classes (as a character vector of class labels) that will be shown in graphical output. A list of 3 should only be provided if the class information is not readily available and should instead be derived using threshold values of a data column (e.g. using 10% and 50% cutoff values of spring sea ice concentration to create 3 classes of sea ice conditions). In such cases, the first list element should contain the column name from which to derive class data, while the second and third elements should contain the threshold values to derive class information from and the desired class labels, respectively.|
| **descriptive_vars** | A character vector of column names from the training dataset to be used as descriptive variables for model training. |
| **scale_vars** | A TRUE/FALSE logical that scales the descriptive variables defined by the **descriptive_vars** argument to percentages of their total when TRUE. Defaults to FALSE, thus assuming that the descriptive variables have already been scaled. |
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
| **plot_cols** | A character vector of colours to use for different classes in the prediction dataset plots. By default, these are generated automatically. |
| **export_results** | A TRUE/FALSE logical. Should model performance metrics and class predictions be exported as an .XLSX file? Defaults to TRUE. |
| **export_plots** | A TRUE/FALSE logical. Should plots be exported as a .PDF file? Defaults to TRUE. |
| **x_lab** | The x-axis label to use for prediction dataset plots. Determined automatically from the data by default. |
| **y_lab** | The y-axis label to use for prediction dataset plots. Determined automatically from the data by default. |
| **y_limit** | A numeric vector of length 2, denoting the y-axis limits for prediction dataset plots. Defaults to NA, such that the axis limits are determined freely for each plot based on the data. |
| **width** | The width of exported .PDF plots. Defaults to 10 inches. |
| **height** | The height of exported .PDF plots. Defaults to 10 inches. |
| **export_path** | The character value for a directory to export results and plots to. Defaults to the working directory. |
| **seed_val** | The random seed value to use for reproducible results with a given dataset. Can be any integer. The default is 42. |

#### Details
Please refer to the [CT Toolkit Vignette](https://github.com/Deniz-Koseoglu/CT-Toolkit/blob/master/Example%20Use%20Vignette.md) for *reproducible* usage examples of all functions. Relevant example training and prediction datasets are available from the [repository](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/Example%20Data).

#### Values
Apart from exporting model performance metrics, classification results and .PDF plots to a chosen directory, within the R environment, the function returns rpart or randomForest objects containing the full and pruned tree models, based on the parameters set by the user. Cross validation results and performance metrics for each model are also returned. This may be changed in the future such that the function does not return anything (i.e. returns NULL) within the R environment, as the exported results encompass all the required and useful output from the function.


### The `runcor_analysis` function
#### Description
A function for computing and visualising running/rolling correlations (Pearson, Kendall, or Spearman) between two specified variables using either absolute values, first absolute differences, and/or first relative differences. This can be supplemented by changepoint analysis to estimate the locations of significant correlation shifts.

#### Usage
```r
runcor_analysis(data_path, separ=",", cor_vars, runcor_permutations="all", cor_method="pearson", cor_window=9, move_by=1,
                confid_lvl=0.95, cor_align="center", output_vars="all", x_var, x_lab, y_limit=NA, pval_corrections="all",
                pval_labels="auto", plot_cols=brewer.pal(5, "Set3"), chgpt_methods="all", export_results=TRUE, 
                print_plots=TRUE, export_plots=TRUE, height=7, width=10, export_path=getwd())
```
#### Arguments
| Argument | Description |
| ------------- |-------------|
| **data_path** | A character value with the full path to a dataset in .CSV format. |
| **separ** | Separator value to use for imported .CSV files. Defaults to a comma.|
| **cor_vars** | A character vector of length 2 denoting the variables to correlate. These must be valid column names within the imported dataset. |
| **runcor_permutations** | Which values to use for calculating running correlations? Any combination of: "Raw Data", "Absolute Differences" and/or "Relative Differences". A value of "all" may be assigned to use all 3 possible data types. |
| **cor_method** | The correlation method to use. One of: "pearson" (the default), "kendall" or "spearman". **NOTE**: As of CT Toolkit version 1.0.0, only "pearson" correlations have gone through extensive testing. |
| **cor_window** | The sample size/window to use for running correlations. Defaults to 9. |
| **move_by** | The step size to use when calculating running correlations. Defaults to 1. |
| **confid_lvl** | A numeric value to use as the confidence level for running correlations. The default is 95% (0.95). |
| **cor_align** | Controls the alignment of running correlations to the original data. Currently the only possible value for this argument is "center". See also the **align** argument for the function **running()** from the [gtools package](https://cran.r-project.org/web/packages/gtools/gtools.pdf).|
| **output_vars** | A character vector of column names from the original dataset to be exported together with correlation coefficients and changepoint results. Defaults to "all", thus exporting the entirety of the originally imported dataset. |
| **x_var** | The name of the column to use for plotting the x-axis (with correlation metric always on the y-axis). |
| **x_lab** | A character value to be used as the x-axis label. Determined automatically from the data by default. |
| **y_limit** | A numeric vector of length 2, denoting the y-axis limits for prediction dataset plots. Defaults to NA, such that the axis limits are determined freely for each plot based on the data. |
| **pval_corrections** | The algorithm(s) to use for adjusting p-values to remove possible positive bias due to the multiple comparison problem. Any combination of: "bonferroni", "BH", "holm", "hochberg", "hommel", and/or "BY". A value of "all" may be assigned to use all possible p-value correction algorithms. |
| **pval_labels** | A character vector of labels to use for each of the p-value correction algorithms passed to the **pval_corrections** argument. The default is "auto", which determines these automatically.|
| **plot_cols** | A character vector of 3 colours to use for different significance levels in the running correlation plots. By default, these are generated automatically. |
| **chgpt_methods** | The changepoint method(s) to use for detecting significant changes in running correlations. Any combination of: "Agglomerative", "Divisive", "CP3O", "KS CP3O", and/or "Univar". A value of "all" may be assigned to carry out all available changepoint analyses. |
| **export_results** | A TRUE/FALSE logical. Should correlation and changepoint results be exported to .XLSX? Defaults to TRUE. |
| **print_plots** | A TRUE/FALSE logical. Should plots be printed within the R environment? Defaults to TRUE. |
| **export_plots** | A TRUE/FALSE logical. Should plots be exported as .PDF files? Defaults to TRUE. |
| **height** | The height of exported .PDF plots. Defaults to 10 inches. |
| **width** | The width of exported .PDF plots. Defaults to 10 inches. |
| **export_path** | The character value for a directory to export results and plots to. Defaults to the working directory. |

#### Details
Please refer to the [CT Toolkit Vignette](https://github.com/Deniz-Koseoglu/CT-Toolkit/blob/master/Example%20Use%20Vignette.md) for *reproducible* usage examples of all functions. Associated example datasets are available from the [repository](https://github.com/Deniz-Koseoglu/CT-Toolkit/tree/master/Example%20Data).

#### Values
Apart from the exported .XLSX and .PDF files containing the relevant function output, runcor_analysis currently returns NULL. This may be changed for future releases.
