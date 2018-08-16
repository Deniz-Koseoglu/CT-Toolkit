easy_rpart <- function(training_data=paste(getwd(), "/Training", sep=""), separ=",", target_var, descriptive_vars, scale_vars=FALSE, descvar_units="%", split_criterion="information", class_method="CART", prune_method="both", tree_number=500, rf_vars=3,
                       cv_method = "repeatedcv", folds=10, replicates=5, train_percent=70, tuning_metric="Kappa", predictions=TRUE, prediction_datasets=paste(getwd(), "/Prediction",sep=""), extension="\\.csv$", plot_predictions=TRUE, prediction_axis=c("Age","SpSIC"),
                       plot_grp_labs=waiver(), tvar_lines=c(10,50), plot_cols=brewer.pal(5, "Set3"), export_results = TRUE, export_plots=TRUE, x_lab=waiver(), y_lab=waiver(), y_limit=NA, width=10, height=10, export_path=getwd(), seed_val=42) {
  
  #Install and load the required libraries
  list.of.packages <- c("rpart", "data.table", "car", "readr", "rattle", "rpart.plot", "e1071", "DMwR", "MLmetrics", "rpartScore", "pROC", "caret",
                        "xlsx", "randomForest", "qpcR", "scales", "ggplot2", "RColorBrewer")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  options(warn=-1)
  
  train_file <- basename(training_data)
  train_data <- as.data.frame(read.csv(as.character(training_data), sep=separ))
  print(paste("Training data successfully loaded from the file:", train_file, sep=" "))
  
  if(scale_vars==TRUE) {
  norm_vars <- as.data.frame(prop.table(as.matrix(train_data[,descriptive_vars]), margin=1)*100)
  colnames(norm_vars) <- desc_varibs <- paste(descriptive_vars, "_normalised", sep="")
  train_data <- cbind(train_data, norm_vars)
  desc_vars <- paste(desc_varibs, collapse=" + ")
  } else if(scale_vars==FALSE) {
    desc_vars <- paste(descriptive_vars, collapse=" + ")
    desc_varibs <- descriptive_vars
  }
  
  if(length(target_var) == 1) {
    
  print(paste("Using the provided target variable \"", target_var, "\"...", sep=""))
  train_data[,target_var] <- group_factor <- factor(train_data[,target_var], levels=unique(train_data[,target_var]))
  cart_formula <- as.formula(paste(target_var, " ~ ", desc_vars, sep=""))
  typeColNum <- grep(as.character(target_var), names(train_data)) #Obtains the column index of the predicted variable (i.e. group assignment) for removal from the dataset prior to plotting the tree.NOT NECESSARY IF YOU EXPLICITLY SPECIFY VARIABLES WHEN YOU INVOKE THE rpart FUNCTION!!!
  
  } else if(length(target_var) == 2 & is.list(target_var)) {
    
    print(paste("Using the provided target variable \"", target_var[[1]], "\"...", sep=""))
    #train_data[,target_var] <- group_factor <- factor(train_data[,target_var], levels=unique(train_data[,target_var]))
    train_data[,target_var[[1]]] <- group_factor <- factor(train_data[,target_var[[1]]], levels=target_var[[2]])
    cart_formula <- as.formula(paste(target_var[[1]], " ~ ", desc_vars, sep=""))
    typeColNum <- grep(as.character(target_var[[1]]), names(train_data)) 
    
  } else if(length(target_var) == 3 & is.list(target_var)) {
    print("Deriving training sample groups from information passed to target_var...")
    group_ranges <- target_var[[2]]
    group_labels <- target_var[[3]]
    train_data <- Data %>% mutate(Group = cut(eval(parse(text=as.character(target_var[[1]]))), group_ranges, labels=group_labels))
    colnames(train_data)[which(names(train_data) == "Group")] <- target_var[[1]]
    #train_data[,c(target_var[1])] <- group_factor <- factor(Data[,c(target_var[1])], levels=unique(Data[,c(target_var[1])]))
    train_data[,c(target_var[[1]])] <- group_factor <- factor(Data[,c(target_var[1])], levels=target_var[[3]])
    cart_formula <- as.formula(paste("Group ~ ", desc_vars, sep=""))
    #typeColNum <- grep("Group", names(train_data))
  }
  
  if(class_method=="CART") {
  Model <- rpart(cart_formula, data=train_data, method="class",control=list(maxdepth=10),parms=list(split = split_criterion), minsplit = 2)
  } else if(class_method=="RF") {
    set.seed(seed_val)
    
    if(is.numeric(rf_vars)) {
    train_data.rf <- randomForest(cart_formula, data=train_data, importance=TRUE, ntree=tree_number, replace=TRUE, mtry = rf_vars)
    } else if(rf_vars=="auto") {
      train_data.rf <- randomForest(cart_formula, data=train_data, importance=TRUE, ntree=tree_number, replace=TRUE, mtry = length(unique(group_factor)))
    }
    train_data.rf
    plot(train_data.rf) # Helps determine an appropriate tree number!
  }

  print(paste("Successfully constructed an unpruned model using the ", class_method, " algorithm...", sep=""))
  
  if(cv_method=="repeatedcv") {
  train_control <- trainControl(method=cv_method, number=folds, repeats=replicates, savePredictions = TRUE, summaryFunction = multiClassSummary, returnResamp="final")
  } else if(cv_method=="LOOCV") {
    train_control <- trainControl(method=cv_method, number=replicates, savePredictions = TRUE, summaryFunction = multiClassSummary, returnResamp="final")
  } else if(cv_method=="LGOCV") {
    train_control <- trainControl(method=cv_method, p=train_percent, number=replicates, savePredictions = TRUE, summaryFunction = multiClassSummary, returnResamp="final")
  } else if(cv_method=="oob") {
    if(class_method != "RF") {
      stop("Out-Of-Bag model validation is only available for the Random Forest method (i.e. change to class_method = \"RF\"")
    } else if(class_method=="RF") {
      train_control <- trainControl(method=cv_method, number=replicates, savePredictions = TRUE, summaryFunction = multiClassSummary, returnResamp="final")
    }
  }
  
  if(class_method=="CART") {
  if(prune_method=="MinCP" | prune_method=="both") {
    optCP <- which.min(Model$cptable[,"xerror"]) #get CP index with lowest cross-validation error, or xerror
    cp <- Model$cptable[optCP, "CP"]
    
    set.seed(seed_val)
    ModelPruned <- prune(Model, cp)
    prp(ModelPruned, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
    title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (pruned by minimum CV error)", sep=""), cex.sub=.6)
    
    set.seed(seed_val)
    tune_grid = expand.grid(cp=cp)
    train.rpart <- train(cart_formula, data=train_data, method="rpart", parms=list(split = split_criterion), trControl=train_control, tuneGrid=tune_grid, metric=tuning_metric)
    conmat_percent_cp <- confusionMatrix(train.rpart, mode="everything")
    train.rpartCM <- data.frame(train.rpart$pred)
    conmat_cp <- confusionMatrix(train.rpartCM$pred,train.rpartCM$obs, mode = "everything")
    
    ImpCM <- varImp(train.rpart, scale = FALSE)
  } 
  
  if(prune_method=="1SE" | prune_method=="both") {
    set.seed(seed_val)
    Model1SE <- rpartXse(cart_formula, data=train_data, method="class",control=list(maxdepth=10),parms=list(split = split_criterion), minsplit = 2)
    prp(Model1SE, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
    title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (pruned via the 1-SE rule)", sep=""), cex.sub=.6)
    
    train.rpart1SE <- train(cart_formula, data=train_data, method="rpart1SE",parms=list(split = split_criterion), trControl=train_control, metric=tuning_metric)
    conmat_percent_1SE <- confusionMatrix(train.rpart1SE, mode="everything")
    train.rpartCM1SE <- data.frame(train.rpart1SE$pred)
    conmat_1SE <- confusionMatrix(train.rpartCM1SE$pred, train.rpartCM1SE$obs, mode="everything")
    
    Imp1SE <- varImp(train.rpart1SE, scale = FALSE)
  } 
  
  if(prune_method=="both") {
    resampsCART <- resamples(list(rpartCM=train.rpart, rpart1SE=train.rpart1SE))
  }
  } else if(class_method=="RF") {
    set.seed(seed_val)
    train.RF <- train(cart_formula, data=train_data, method="rf", trControl=train_control, metric=tuning_metric)
    #print(train.RF)
    conmat_percent_RF <- confusionMatrix(train.RF, mode="everything")
    train.CM_RF <- data.frame(train.RF$pred)
    conmat_RF <- confusionMatrix(train.CM_RF$pred, train.CM_RF$obs, mode="everything")
    
    ImpRF <- train_data.rf$importance
  }
  
  print("Model pruned via the chosen pruning and cross-validation procedures...")
  
  if(predictions==TRUE) {
    setwd(prediction_datasets)
    prediction_data_files <- list.files(pattern = extension)
    
    prediction_data <- lapply(prediction_data_files, fread, select=c(prediction_axis,descriptive_vars))
    names(prediction_data) <- gsub(paste("\\.csv$"), "", prediction_data_files)
    prediction_data <- lapply(prediction_data, as.data.frame, stringsAsFactors=FALSE)
    
    if(scale_vars==TRUE) {
      for(i in 1:length(prediction_data)) {
        norm_pred_vars <- as.data.frame(prop.table(as.matrix(prediction_data[[i]]), margin=1)*100) #[[descriptive_vars]]
        colnames(norm_pred_vars) <- paste(descriptive_vars, "_normalised", sep="")
        prediction_data[[i]] <- cbind(prediction_data[,c(prediction_axis)], norm_pred_vars)
        names(prediction_data)[i] <- gsub(paste("\\.csv$"), "", prediction_data_files[i])
      }
    }
    #return(list(ModelRes=Model1SE, Preds=prediction_data))}}
    
    if(class_method=="CART") {
    if(prune_method=="MinCP") {
      if(length(prediction_data)==1) {
        prediction_frame <- as.data.frame(prediction_data[[1]])
        Core_Predictions <- as.data.frame(predict(ModelPruned, newdata=prediction_frame[,desc_varibs], type="class"))
        colnames(Core_Predictions) <- paste(names(prediction_data), "_minCP", sep="")
        Prediction_Results <- Core_Predictions
      } else if(length(prediction_data)!=1) {
        Core_Predictions <- lapply(prediction_data, function(x){as.data.frame(predict(ModelPruned, newdata=x[,desc_varibs], type="class"))})
        names(Core_Predictions) <- paste(names(prediction_data), "_minCP", sep="")
        #Core_Cbind <- Core_Predictions
        #names(Core_Predictions) <- names(prediction_data)
        Prediction_Results <- do.call(qpcR:::cbind.na, Core_Predictions)
        colnames(Prediction_Results) <- names(Core_Predictions)
        Prediction_Results[] <- lapply(Prediction_Results, as.character)
      }
    } else if(prune_method=="1SE") {
      if(length(prediction_data)==1) {
        prediction_frame <- as.data.frame(prediction_data[[1]])
        Core_Predictions <- as.data.frame(predict(Model1SE, newdata=prediction_frame[,desc_varibs], type="class"))
        colnames(Core_Predictions) <- paste(names(prediction_data), "_1SE", sep="")
        Prediction_Results <- Core_Predictions
      } else if(length(prediction_data)!=1) {
    Core_Predictions <- lapply(prediction_data, function(x){as.data.frame(predict(Model1SE, newdata=x[,desc_varibs], type="class"))})
    names(Core_Predictions) <- paste(names(prediction_data), "_1SE", sep="")
    #Core_Cbind <- Core_Predictions
    #names(Core_Predictions) <- names(prediction_data)
    Prediction_Results <- do.call(qpcR:::cbind.na, Core_Predictions)
    colnames(Prediction_Results) <- names(Core_Predictions)
    Prediction_Results[] <- lapply(Prediction_Results, as.character)
    Prediction_Results[is.na(Prediction_Results)] <- ""
      }
    } else if(prune_method=="both") {
    model_list <- list(ModelPruned, Model1SE)
    
    if(length(prediction_data)==1) {
      prediction_frame <- as.data.frame(prediction_data[[1]])
      Core_Predictions <- as.data.frame(predict(model_list, newdata=prediction_frame[,desc_varibs], type="class"))
      colnames(Core_Predictions) <- c(paste(names(prediction_data), "_minCP", sep=""), paste(names(prediction_data), "_1SE", sep=""))
      Prediction_Results <- Core_Predictions
    } else if(length(prediction_data)!=1) {
    Core_Predictions <- lapply(prediction_data, function(x){as.data.frame(predict(model_list, newdata=x[,desc_varibs], type="class"))})
    for(i in 1:length(Core_Predictions)) {
      colnames(Core_Predictions[[i]]) <- c(paste(names(Core_Predictions)[i],"_minCP", sep=""), paste(names(Core_Predictions)[i],"_1SE", sep=""))
    }
    backup_names <- names(Core_Predictions)
    names(Core_Predictions) <- NULL
    Prediction_Results <- do.call(qpcR:::cbind.na, Core_Predictions)
    names(Core_Predictions) <- backup_names
    #Core_Cbind <- Core_Predictions
    #names(Core_Predictions) <- names(prediction_data)
    #Prediction_Results <- as.data.frame(sapply(Prediction_Results, as.character))
    Prediction_Results[] <- lapply(Prediction_Results, as.character)
    Prediction_Results[is.na(Prediction_Results)] <- ""
    }
    }
    } else if(class_method=="RF") {
      Core_Predictions <- lapply(prediction_data, function(x){as.data.frame(predict(train_data.rf, newdata=x[,desc_varibs]))})
      names(Core_Predictions) <- paste(names(prediction_data), "_RF", sep="")
      #Core_Cbind <- Core_Predictions
      #names(Core_Predictions) <- names(prediction_data)
      Prediction_Results <- do.call(qpcR:::cbind.na, Core_Predictions)
      colnames(Prediction_Results) <- names(Core_Predictions)
      Prediction_Results[] <- lapply(Prediction_Results, as.character)
      Prediction_Results[is.na(Prediction_Results)] <- ""
    }
    
    #keys <- unique(c(names(prediction_data), names(Core_Cbind)))
    #setNames(mapply(c, lst1[keys], lst2[keys]), keys)
    
    print_val <- paste("CT prediction successfully completed using the following file(s): ", prediction_data_files[1], sep="")
    print(paste(c(print_val, prediction_data_files[-1]), collapse=", "))
  }
  #return(Prediction_Results)}
  
  # Collecting surface sediment (training data) classification after cross validation
  if(class_method=="CART") {
  if(prune_method=="MinCP" | prune_method=="both") {
    surface_predictions_mincp <- split(train.rpart$pred, rep(1:replicates, each=nrow(train_data)))
    for(i in 1:length(surface_predictions_mincp)) {
      surface_predictions_mincp[[i]][,"pred"] <- as.character(surface_predictions_mincp[[i]][,"pred"])
    }
    surface_predictions_mincp <- lapply(surface_predictions_mincp, function(x) {x[order(x[,c("rowIndex")]), c("pred")]})
    surface_results_mincp <- do.call(cbind, surface_predictions_mincp)
    colnames(surface_results_mincp) <- paste("Repeat #", 1:replicates, " (min CV error)", sep="")
    surface_res_mincp <- cbind(train_data, surface_results_mincp)
  }
  
  if(prune_method=="1SE" | prune_method=="both") {
    surface_predictions_1SE <- split(train.rpart1SE$pred, rep(1:replicates, each=nrow(train_data)))
    for(i in 1:length(surface_predictions_1SE)) {
      surface_predictions_1SE[[i]][,"pred"] <- as.character(surface_predictions_1SE[[i]][,"pred"])
    }
    surface_predictions_1SE <- lapply(surface_predictions_1SE, function(x) {x[order(x[,c("rowIndex")]), c("pred")]})
    surface_results_1SE <- do.call(cbind, surface_predictions_1SE)
    colnames(surface_results_1SE) <- paste("Repeat #", 1:replicates, " (1-SE rule)", sep="")
    surface_res_1SE <- cbind(train_data, surface_results_1SE)
  } 
  
  if(prune_method=="both") {
    surface_results_list <- list(train_data, surface_results_mincp, surface_results_1SE)
    surface_results_both <- do.call(cbind, surface_results_list)
  }
  } else if(class_method=="RF") {
    train.RFF <- train.RF$pred
    
    if(is.numeric(rf_vars)) {
    RF_results <- train.RFF[train.RFF[,"mtry"] %in% rf_vars,]
    } else if(rf_vars=="auto") {
      RF_results <- train.RFF[train.RFF[,"mtry"] %in% as.numeric(train.RF$bestTune),]
    }
    
    surface_predictions_RF <- split(RF_results, rep(1:replicates, each=nrow(train_data)))
    for(i in 1:length(surface_predictions_RF)) {
      surface_predictions_RF[[i]][,"pred"] <- as.character(surface_predictions_RF[[i]][,"pred"])
    }
    surface_predictions_RF <- lapply(surface_predictions_RF, function(x) {x[order(x[,c("rowIndex")]), c("pred")]})
    surface_results_RF <- do.call(cbind, surface_predictions_RF)
    
    if(is.numeric(rf_vars)) {
    colnames(surface_results_RF) <- paste("Repeat #", 1:replicates, " (RF, mtry=", rf_vars ,")", sep="")
    } else if(rf_vars=="auto") {
      colnames(surface_results_RF) <- paste("Repeat #", 1:replicates, " (RF, mtry=", as.numeric(train.RF$bestTune) ,")", sep="")
    }
    
    surface_results_RF <- cbind(train_data, surface_results_RF)
  }
  print("Cross-validated performance metrics compiled from the training dataset...")
  #return(list(surface_res_mincp, surface_res_1SE, surface_results_both))}
  
  # Exporting results as an .xlsx file
  if(export_results==TRUE) {
    if(class_method=="CART" & prune_method=="MinCP") {
  CT_Results <- createWorkbook()
  sheet1 <- createSheet(CT_Results, paste("CT model ", "(", prune_method, ", ", "n=", replicates, ")", sep=""))
  addDataFrame(as.data.frame.matrix(conmat_percent_cp$table), sheet=sheet1, startColumn=1, startRow=3, row.names=TRUE)
  addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
  addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
  addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
  
  addDataFrame(as.data.frame.matrix(conmat_cp$table), sheet=sheet1, startColumn=4+length(unique(group_factor)), startRow=3, row.names=TRUE)
  addDataFrame("Confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
  addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 5+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
  addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
  
  overall_perf_met <- data.frame(conmat_cp$overall, row.names=c("Accuracy", "Kappa", "Lower Accuracy CI", "Upper Accuracy CI", "No Information Rate", "Accuracy p-value", "McNemar's p-value"))
  colnames(overall_perf_met) <- c("Brief Performance Summary")
  addDataFrame(overall_perf_met, sheet=sheet1, startColumn = 1, startRow = 6+length(unique(group_factor)))
  
  detailed_perf_met <- as.data.frame(cbind(t(data.frame(train.rpart$results[,2:12])), t(data.frame(train.rpart$results[,13:23]))))
  rownames(detailed_perf_met) <- c("Accuracy", "Kappa", "F1", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value", "Precision", "Recall", "Detection Rate", "Balanced Accuracy")
  colnames(detailed_perf_met) <- c("Mean", "SD")
  addDataFrame(detailed_perf_met, sheet=sheet1, startRow= 6+length(unique(group_factor)), startColumn = 5, col.names=TRUE, row.names=TRUE)
  
  perclass_perf_met <- as.data.frame(t(conmat_cp$byClass))
  addDataFrame(perclass_perf_met, sheet=sheet1, startColumn = 9, startRow = 6+length(unique(group_factor)), col.names=TRUE, row.names=TRUE)
  
  var_imp_table <- as.data.frame(ImpCM$importance)
  colnames(var_imp_table) <- c("Variable importance")
  addDataFrame(var_imp_table, sheet=sheet1, startColumn = 11+length(unique(group_factor)), startRow = 6+length(unique(group_factor)), col.names = TRUE, row.names = TRUE)
  
  cp_tab_res <- as.data.frame(Model$cptable)
  colnames(cp_tab_res) <- c("CP parameter", "Tree splits", "Relative Error", "Cross-Validation Error", "CV error SD")
  addDataFrame(cp_tab_res, sheet=sheet1, startColumn = 1, startRow = 19+length(unique(group_factor)), row.names = FALSE, col.names = TRUE)
  
  sheet2 <- createSheet(CT_Results, paste("Training (minCP, n=", replicates, ")", sep=""))
  addDataFrame(surface_res_mincp, sheet=sheet2, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
  
  if(predictions==TRUE) {
  sheet3 <- createSheet(CT_Results, paste("Predictions (minCP, n=", replicates, ")", sep=""))
  addDataFrame(Prediction_Results, sheet=sheet3, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
  }
  
  saveWorkbook(CT_Results, paste(export_path,"/CT Results ", format(Sys.time(), "%Y-%m-%d %I-%p"), ".xlsx", sep=""))
  
  } else if(class_method=="CART" & prune_method=="1SE") {
    CT_Results <- createWorkbook()
    sheet1 <- createSheet(CT_Results, paste("CT model ", "(", prune_method, ", ", "n=", replicates, ")", sep=""))
    addDataFrame(as.data.frame.matrix(conmat_percent_1SE$table), sheet=sheet1, startColumn=1, startRow=3, row.names=TRUE)
    addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
    
    addDataFrame(as.data.frame.matrix(conmat_1SE$table), sheet=sheet1, startColumn=4+length(unique(group_factor)), startRow=3, row.names=TRUE)
    addDataFrame("Confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 5+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    
    overall_perf_met <- data.frame(conmat_1SE$overall, row.names=c("Accuracy", "Kappa", "Lower Accuracy CI", "Upper Accuracy CI", "No Information Rate", "Accuracy p-value", "McNemar's p-value"))
    colnames(overall_perf_met) <- c("Brief Performance Summary")
    addDataFrame(overall_perf_met, sheet=sheet1, startColumn = 1, startRow = 6+length(unique(group_factor)))
    
    detailed_perf_met <- as.data.frame(cbind(t(data.frame(train.rpart1SE$results[,2:12])), t(data.frame(train.rpart1SE$results[,13:23]))))
    rownames(detailed_perf_met) <- c("Accuracy", "Kappa", "F1", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value", "Precision", "Recall", "Detection Rate", "Balanced Accuracy")
    colnames(detailed_perf_met) <- c("Mean", "SD")
    addDataFrame(detailed_perf_met, sheet=sheet1, startRow= 6+length(unique(group_factor)), startColumn = 5, col.names=TRUE, row.names=TRUE)
    
    perclass_perf_met <- as.data.frame(t(conmat_1SE$byClass))
    addDataFrame(perclass_perf_met, sheet=sheet1, startColumn = 9, startRow = 6+length(unique(group_factor)), col.names=TRUE, row.names=TRUE)
    
    var_imp_table <- as.data.frame(Imp1SE$importance)
    colnames(var_imp_table) <- c("Variable importance")
    addDataFrame(var_imp_table, sheet=sheet1, startColumn = 11+length(unique(group_factor)), startRow = 6+length(unique(group_factor)), col.names = TRUE, row.names = TRUE)
    
    cp_tab_res <- as.data.frame(Model$cptable)
    colnames(cp_tab_res) <- c("CP parameter", "Tree splits", "Relative Error", "Cross-Validation Error", "CV error SD")
    addDataFrame(cp_tab_res, sheet=sheet1, startColumn = 1, startRow = 19+length(unique(group_factor)), row.names = FALSE, col.names = TRUE)
    
    sheet2 <- createSheet(CT_Results, paste("Training (1SE rule, n=", replicates, ")", sep=""))
    addDataFrame(surface_res_1SE, sheet=sheet2, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    
    if(predictions==TRUE) {
    sheet3 <- createSheet(CT_Results, paste("Predictions (1SE rule, n=", replicates, ")", sep=""))
    addDataFrame(Prediction_Results, sheet=sheet3, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    }
    saveWorkbook(CT_Results, paste(export_path,"/CT Results ", format(Sys.time(), "%Y-%m-%d %I-%p"), ".xlsx", sep=""))
  } else if(class_method=="CART" & prune_method=="both") {
    CT_Results <- createWorkbook()
    sheet1 <- createSheet(CT_Results, paste("CT model ", "(minCP, n=", replicates, ")", sep=""))
    addDataFrame(as.data.frame.matrix(conmat_percent_cp$table), sheet=sheet1, startColumn=1, startRow=3, row.names=TRUE)
    addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
    
    addDataFrame(as.data.frame.matrix(conmat_cp$table), sheet=sheet1, startColumn=4+length(unique(group_factor)), startRow=3, row.names=TRUE)
    addDataFrame("Confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 5+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    
    overall_perf_met_cp <- data.frame(conmat_cp$overall, row.names=c("Accuracy", "Kappa", "Lower Accuracy CI", "Upper Accuracy CI", "No Information Rate", "Accuracy p-value", "McNemar's p-value"))
    colnames(overall_perf_met_cp) <- c("Brief Performance Summary")
    addDataFrame(overall_perf_met_cp, sheet=sheet1, startColumn = 1, startRow = 6+length(unique(group_factor)))
    
    detailed_perf_met_cp <- as.data.frame(cbind(t(data.frame(train.rpart$results[,2:12])), t(data.frame(train.rpart$results[,13:23]))))
    rownames(detailed_perf_met_cp) <- c("Accuracy", "Kappa", "F1", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value", "Precision", "Recall", "Detection Rate", "Balanced Accuracy")
    colnames(detailed_perf_met_cp) <- c("Mean", "SD")
    addDataFrame(detailed_perf_met_cp, sheet=sheet1, startRow= 6+length(unique(group_factor)), startColumn = 5, col.names=TRUE, row.names=TRUE)
    
    perclass_perf_met_cp <- as.data.frame(t(conmat_cp$byClass))
    addDataFrame(perclass_perf_met_cp, sheet=sheet1, startColumn = 9, startRow = 6+length(unique(group_factor)), col.names=TRUE, row.names=TRUE)
    
    var_imp_table_cp <- as.data.frame(ImpCM$importance)
    colnames(var_imp_table_cp) <- c("Variable importance")
    addDataFrame(var_imp_table_cp, sheet=sheet1, startColumn = 11+length(unique(group_factor)), startRow = 6+length(unique(group_factor)), col.names = TRUE, row.names = TRUE)
    
    cp_tab_res_cp <- as.data.frame(Model$cptable)
    colnames(cp_tab_res_cp) <- c("CP parameter", "Tree splits", "Relative Error", "Cross-Validation Error", "CV error SD")
    addDataFrame(cp_tab_res_cp, sheet=sheet1, startColumn = 1, startRow = 19+length(unique(group_factor)), row.names = FALSE, col.names = TRUE)
    
    
    sheet2 <- createSheet(CT_Results, paste("CT model (1SE rule, n=", replicates, ")", sep=""))
    addDataFrame(as.data.frame.matrix(conmat_percent_1SE$table), sheet=sheet2, startColumn=1, startRow=3, row.names=TRUE)
    addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet2, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet2, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet2, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
    
    addDataFrame(as.data.frame.matrix(conmat_1SE$table), sheet=sheet2, startColumn=4+length(unique(group_factor)), startRow=3, row.names=TRUE)
    addDataFrame("Confusion matrix (accuracy across CV resamples)", sheet=sheet2, startRow = 1, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet2, startRow = 2, startColumn = 5+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet2, startRow = 3, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    
    overall_perf_met_1SE <- data.frame(conmat_1SE$overall, row.names=c("Accuracy", "Kappa", "Lower Accuracy CI", "Upper Accuracy CI", "No Information Rate", "Accuracy p-value", "McNemar's p-value"))
    colnames(overall_perf_met_1SE) <- c("Brief Performance Summary")
    addDataFrame(overall_perf_met_1SE, sheet=sheet2, startColumn = 1, startRow = 6+length(unique(group_factor)))
    
    detailed_perf_met_1SE <- as.data.frame(cbind(t(data.frame(train.rpart1SE$results[,2:12])), t(data.frame(train.rpart1SE$results[,13:23]))))
    rownames(detailed_perf_met_1SE) <- c("Accuracy", "Kappa", "F1", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value", "Precision", "Recall", "Detection Rate", "Balanced Accuracy")
    colnames(detailed_perf_met_1SE) <- c("Mean", "SD")
    addDataFrame(detailed_perf_met_1SE, sheet=sheet2, startRow= 6+length(unique(group_factor)), startColumn = 5, col.names=TRUE, row.names=TRUE)
    
    perclass_perf_met_1SE <- as.data.frame(t(conmat_1SE$byClass))
    addDataFrame(perclass_perf_met_1SE, sheet=sheet2, startColumn = 9, startRow = 6+length(unique(group_factor)), col.names=TRUE, row.names=TRUE)
    
    var_imp_table_1SE <- as.data.frame(Imp1SE$importance)
    colnames(var_imp_table_1SE) <- c("Variable importance")
    addDataFrame(var_imp_table_1SE, sheet=sheet2, startColumn = 11+length(unique(group_factor)), startRow = 6+length(unique(group_factor)), col.names = TRUE, row.names = TRUE)
    
    cp_tab_res_1SE <- as.data.frame(Model$cptable)
    colnames(cp_tab_res_1SE) <- c("CP parameter", "Tree splits", "Relative Error", "Cross-Validation Error", "CV error SD")
    addDataFrame(cp_tab_res_1SE, sheet=sheet2, startColumn = 1, startRow = 19+length(unique(group_factor)), row.names = FALSE, col.names = TRUE)
    
    sheet3 <- createSheet(CT_Results, paste("Training (minCP and 1SE, n=", replicates, ")", sep=""))
    addDataFrame(surface_results_both, sheet=sheet3, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    
    if(predictions==TRUE) {
    sheet4 <- createSheet(CT_Results, paste("Predictions (minCP and 1SE, n=", replicates, ")", sep=""))
    addDataFrame(Prediction_Results, sheet=sheet4, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    }
    saveWorkbook(CT_Results, paste(export_path,"/CT Results ", format(Sys.time(), "%Y-%m-%d %I-%p"), ".xlsx", sep=""))
  } else if(class_method=="RF") {
    CT_Results <- createWorkbook()
    sheet1 <- createSheet(CT_Results, paste("RF Model (n=", replicates, ")", sep=""))
    addDataFrame(as.data.frame.matrix(conmat_percent_RF$table), sheet=sheet1, startColumn=1, startRow=3, row.names=TRUE)
    addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
    
    addDataFrame(as.data.frame.matrix(conmat_RF$table), sheet=sheet1, startColumn=4+length(unique(group_factor)), startRow=3, row.names=TRUE)
    addDataFrame("Confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 5+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 4+length(unique(group_factor)), col.names = FALSE, row.names = FALSE)
    
    overall_perf_met_RF <- data.frame(conmat_RF$overall, row.names=c("Accuracy", "Kappa", "Lower Accuracy CI", "Upper Accuracy CI", "No Information Rate", "Accuracy p-value", "McNemar's p-value"))
    colnames(overall_perf_met_RF) <- c("Brief Performance Summary")
    addDataFrame(overall_perf_met_RF, sheet=sheet1, startColumn = 1, startRow = 6+length(unique(group_factor)))
    
    detailed_perf_met_RF <- as.data.frame(cbind(t(data.frame(train.RF$results[,2:12])), t(data.frame(train.RF$results[,13:23]))))
    rownames(detailed_perf_met_RF) <- c("Accuracy", "Kappa", "F1", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value", "Precision", "Recall", "Detection Rate", "Balanced Accuracy")
    colnames(detailed_perf_met_RF) <- c(paste("Mean (mtry=", train.RF$results[1:nrow(train.RF$results),1], ")", sep=""), paste("SD (mtry=", train.RF$results[1:nrow(train.RF$results),1], ")", sep=""))
    addDataFrame(detailed_perf_met_RF, sheet=sheet1, startRow= 6+length(unique(group_factor)), startColumn = 5, col.names=TRUE, row.names=TRUE)
    
    perclass_perf_met_RF <- as.data.frame(t(conmat_RF$byClass))
    addDataFrame(perclass_perf_met_RF, sheet=sheet1, startColumn = 9+2*(nrow(train.RF$results)-1), startRow = 6+length(unique(group_factor)), col.names=TRUE, row.names=TRUE)
    
    var_imp_table_rf <- as.data.frame(ImpRF)
    #colnames(var_imp_table_rf) <- c("Variable importance")
    addDataFrame(var_imp_table_rf, sheet=sheet1, startColumn = 11+length(unique(group_factor))+2*(nrow(train.RF$results)-1), startRow = 6+length(unique(group_factor)), col.names = TRUE, row.names = TRUE)
    
    #cp_tab_res <- as.data.frame(Model$cptable)
    #colnames(cp_tab_res) <- c("CP parameter", "Tree splits", "Relative Error", "Cross-Validation Error", "CV error SD")
    #addDataFrame(cp_tab_res, sheet=sheet1, startColumn = 1, startRow = 19+length(unique(group_factor)), row.names = FALSE, col.names = TRUE)
    if(is.numeric(rf_vars)) {
    sheet2 <- createSheet(CT_Results, paste("Training (RF, mtry=", rf_vars, ", n=", replicates, ")", sep=""))
    } else if(rf_vars=="auto"){
      sheet2 <- createSheet(CT_Results, paste("Training (RF, mtry=", train.RF$bestTune, ", n=", replicates, ")", sep=""))
    }
    addDataFrame(surface_results_RF, sheet=sheet2, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    
    if(predictions==TRUE) {
      if(is.numeric(rf_vars)) {
    sheet3 <- createSheet(CT_Results, paste("Predictions (RF, mtry=", rf_vars, ", n=", replicates, ")", sep=""))
      } else if(rf_vars=="auto") {
        sheet3 <- createSheet(CT_Results, paste("Predictions (RF, mtry=", train.RF$bestTune, ", n=", replicates, ")", sep=""))
      }
      addDataFrame(Prediction_Results, sheet=sheet3, startColumn = 1, startRow = 1, row.names=FALSE, col.names=TRUE)
    }
    saveWorkbook(CT_Results, paste(export_path,"/CT Results ", format(Sys.time(), "%Y-%m-%d %I-%p"), ".xlsx", sep=""))
  }
  }
  
  print(paste("Model results exported as an .xlsx file to: ", export_path, sep=""))
  
  if(plot_predictions==TRUE & predictions==TRUE) {
    
    CorePltList <- list()
    
    for(i in names(prediction_data)) {
      class_graph_data <- Prediction_Results[,grepl(i , colnames(Prediction_Results))]
      class_graph_data <- class_graph_data[!apply(class_graph_data == "", 1, all),]
      graph_data <- as.data.frame(cbind(prediction_data[[i]], class_graph_data))
      
      for(j in colnames(class_graph_data)) {
      
      if(length(target_var) == 1) {
        graph_data[,j] <- factor(graph_data[,j], levels=unique(graph_data[,j]))
      } else if(length(target_var) == 2 & is.list(target_var)) {
        graph_data[,j] <- factor(graph_data[,j], levels=target_var[[2]])
      }
      
      pltName <- paste(j, sep='')
      #graph_data[[i]]["Class"] <- factor(graph_data[[i]]["Class"], levels=unique(graph_data[[1]]))
      CorePltList[[pltName]] <- ggplot(data=graph_data, aes_string(x=prediction_axis[1], y=prediction_axis[2], colour=c(j), fill=c(j), shape=c(j))) +
        #geom_ribbon(data=graph_data[[i]], aes(x=Age/1000, ymin=min_err,ymax=max_err), colour="grey50", fill=c("#00AFBB"), alpha=0.1, inherit.aes=FALSE) +
        geom_hline(colour="grey55", lty=2, lwd=0.7, yintercept = tvar_lines) +
        geom_path(colour="grey15", group=1, lwd=0.7) + # geom_path follows the order of data passed to it
        geom_point(colour="black", size=2.5) +
        #geom_smooth(method="lm", formula=my.formula,show.legend=FALSE, size=0.4) +
        scale_shape_manual(values=c(21,22,23), name=NULL, labels=plot_grp_labs) +
        scale_colour_manual(values=plot_cols) +
        scale_fill_manual(values=plot_cols, name=NULL, labels=plot_grp_labs) +
        scale_x_continuous(breaks=pretty_breaks(n=10)) +
        scale_y_continuous(breaks=pretty_breaks(n=10)) +
        theme(axis.text.x = element_text(colour="black", angle = 0, size=10, margin=ggplot2::margin(c(0.1,0.1,0.1,0.1), unit="cm")),
              axis.text.y = element_text(colour="black", size=10, margin=ggplot2::margin(c(0.1,0.1,0.1,0.1), unit="cm")),
              axis.ticks.length=unit(0.1, "cm"),
              #axis.ticks=element_line(),
              plot.margin = grid::unit(c(1, 1, 1, 1), "lines"),
              axis.line=element_line(colour="grey75"),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_blank(),
              #panel.grid.major.y=element_line(colour="grey65",linetype="solid"),
              panel.grid.major=element_blank(),
              panel.border=element_rect(fill=NA, colour="grey40"),
              panel.background=element_blank(),
              axis.title.x=element_text(colour="black", margin=ggplot2::margin(t=0.1, unit="cm")),
              axis.title.y=element_text(colour="black", margin=ggplot2::margin(r=0.1, unit="cm")),
              legend.position="bottom", legend.direction="horizontal",
              legend.key=element_blank(),
              legend.margin=ggplot2::margin(t=0, unit="cm"),
              legend.text=element_text(size=11),
              legend.text.align=0,
              legend.title=element_text(face="bold")) + 
        labs(x=x_lab, y=y_lab, title=j, fill=NULL)
      
      if(!any(y_limit=="none") & !any(is.na(y_limit))) {
        CorePltList[[pltName]] <- CorePltList[[pltName]] + coord_cartesian(ylim = y_limit) 
      }
      
    }
    }
    print("Printing classification plots for imported prediction datasets...")
    print(CorePltList)
    #return(CorePltList)
  }
  
  if(export_plots==TRUE) {
    pdf(paste(export_path, "/CT_Plots ", format(Sys.time(), "%Y-%m-%d %I-%p") , ".pdf", sep=""),
        width=width, height=height)
    
    if(class_method=="CART") {
    prp(Model, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
    title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (full tree)", sep=""), cex.sub=.6)
    plotcp(Model, upper="splits")
    }
    
    if(class_method=="CART" & prune_method=="MinCP") {
    prp(ModelPruned, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
    title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (CP-pruned tree)", sep=""), cex.sub=.6)
    #plot(ImpCM)
    print(ggplot(ImpCM, mapping = NULL,
           top = dim(ImpCM$importance)[1], environment = NULL))
    dev.off()
    
    } else if(class_method=="CART" & prune_method=="1SE") {
      prp(Model1SE, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
      title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (1SE-pruned tree)", sep=""), cex.sub=.6)
      #plot(Imp1SE)
      print(ggplot(Imp1SE, mapping = NULL,
                   top = dim(Imp1SE$importance)[1], environment = NULL))
      dev.off()
      
    } else if(class_method=="CART" & prune_method=="both") {
      prp(ModelPruned, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
      title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (CP-pruned tree)", sep=""), cex.sub=.6)
      #plot(ImpCM)
      print(ggplot(ImpCM, mapping = NULL,
                   top = dim(ImpCM$importance)[1], environment = NULL))
      prp(Model1SE, type=2, extra=2, under = FALSE, snip = FALSE, legend.x = NA, box.palette="GnYlRd", branch.lty=3, shadow.col="gray", under.cex=0.8, branch=0.2, digits=2, varlen=0, faclen=0, split.suffix=paste(" ", descvar_units, sep=""))
      title(paste("Classifying ", length(unique(group_factor)), " sea ice categories (1SE-pruned tree)", sep=""), cex.sub=.6)
      #plot(Imp1SE)
      print(ggplot(Imp1SE, mapping = NULL,
                   top = dim(Imp1SE$importance)[1], environment = NULL))
      dev.off()
      
    } else if(class_method=="RF") {
      plot(train_data.rf)
      varImpPlot(train_data.rf)
      dev.off()
    }
    
    if(plot_predictions==TRUE & export_plots==TRUE) {
      pdf(paste(export_path, "/CT_NewPredictions ", format(Sys.time(), "%Y-%m-%d %I-%p") , ".pdf", sep=""),
          width=width, height=height)
      print(CorePltList)
      dev.off()
    }
  }
  
  print(paste("Relevant plots exported as a .pdf file to: ", export_path, sep=""))
  print("Model building and testing completed!")
  
  options(warn=0)
  
  if(class_method=="CART" & prune_method=="MinCP") {
  return(list(Model, ModelPruned, train.rpart))
  } else if(class_method=="CART" & prune_method=="1SE") {
    return(list(Model, Model1SE, train.rpart1SE))
  } else if(class_method=="CART" & prune_method=="both") {
    return(list(Model, ModelPruned, Model1SE, train.rpart, train.rpart1SE))
  } else if(class_method=="RF") {
    return(list(train_data.rf, train.RF))
  }
}



runcor_analysis <- function(data_path, separ=",", cor_vars, runcor_permutations="all", cor_method="pearson", cor_window=9, move_by=1,
                            confid_lvl=0.95, cor_align="center", output_vars="all", x_var, x_lab, y_limit=NA, pval_corrections="all",
                            pval_labels="auto", plot_cols=brewer.pal(5, "Set3"), chgpt_methods="all", export_results=TRUE, 
                            print_plots=TRUE, export_plots=TRUE, height=7, width=10, export_path=getwd()) {
  
  #Install and load the required libraries
  list.of.packages <- c("readr", "gtools", "ecp", "changepoint", "ggplot2", "RColorBrewer", "qpcR", "xlsx")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  #library(readr)
  #library(gtools)
  #library(ecp)
  #library(changepoint)
  #library(ggplot2)
  #library(RColorBrewer)
  #library(qpcR)
  #library(xlsx)
  
  main_file <- basename(data_path)
  main_data <- as.data.frame(read.csv(as.character(data_path), sep=separ))
  print(paste("The following data file was successfully loaded:", main_file, sep=" "))
  
  if((cor_window %% 2) == 0) {
    stop("Correlation window must be an odd number for center alignment! Correct the cor_window argument.")
  } else {
    w_lb <- as.integer(1+((cor_window-1)/2))
    w_ub <- as.integer((cor_window-1)/2)
  }
  
  vec_1 <- c(main_data[,cor_vars[1]])
  vec_2 <- c(main_data[,cor_vars[2]])
  
  output_list <- list()
  
  print("Calculating running correlations...")
  if(c("Raw Data") %in% runcor_permutations | runcor_permutations=="all") {
    corr_table <- running(vec_1, vec_2, fun=cor.test, method=cor_method, conf.level=confid_lvl, alternative="two.sided", width=cor_window, by=move_by, align=cor_align, pad=FALSE)
    cor_summary <- as.data.frame(cbind(estimate=unlist(corr_table[4,]), p_value=corr_table[3,]))
    sample_output <- apply(cbind(main_data[w_lb:sum(nrow(main_data)-w_ub),output_vars], cor_summary), 2, as.character)
    output_list[["Raw Data"]] <- sample_output[complete.cases(sample_output),]
    #plot(sample_data$Age[5:sum(nrow(sample_data)-4)], cor_summary$estimate, ylab="Correlation", type="l", xlab="Year CE")
  }
  
  if(c("Absolute Differences") %in% runcor_permutations | runcor_permutations=="all") {
    chg_1 <- diff(vec_1)
    chg_2 <- diff(vec_2)
    chg_data <- cbind(as.data.frame(chg_1), as.data.frame(chg_2))
    chg_total_data <- cbind(main_data[2:nrow(main_data),], chg_data)
    chg_total_data <- chg_total_data[complete.cases(chg_total_data),]
    
    corr_chg <- running(chg_total_data$chg_1, chg_total_data$chg_2, fun=cor.test, method=cor_method, conf.level=confid_lvl, alternative="two.sided", width=cor_window, by=move_by, align=cor_align, pad=FALSE)
    cor_abs_diff <- as.data.frame(cbind(estimate=unlist(corr_chg[4,]), p_value=corr_chg[3,]))
    abs_diff_output <- apply(cbind(chg_total_data[w_lb:sum(nrow(chg_total_data)-w_ub),output_vars], cor_abs_diff), 2, as.character)
    output_list[["Absolute Differences"]] <- abs_diff_output[complete.cases(abs_diff_output),]
    #plot(chg_total_data$Age[5:sum(nrow(chg_total_data)-4)], cor_abs_diff$estimate, ylab="Correlation", type="l", xlab="Year CE")
  }
  
  if(c("Relative Differences") %in% runcor_permutations | runcor_permutations=="all") {
    rel_1 <- diff(vec_1)/vec_1[-length(vec_1)] 
    rel_2 <- diff(vec_2)/vec_2[-length(vec_2)]
    rel_1[rel_1 %in% c("NaN", "Inf")] <- NA
    rel_2[rel_2 %in% c("NaN", "Inf")] <- NA
    
    rel_data <- cbind(as.data.frame(rel_1), as.data.frame(rel_2))
    total_data <- cbind(main_data[2:nrow(main_data),], rel_data)
    total_data <- total_data[complete.cases(total_data),]
    
    corr_diff <- running(total_data$rel_1, total_data$rel_2, fun=cor.test, method=cor_method, conf.level=confid_lvl, alternative="two.sided", width=cor_window, by=move_by, align=cor_align, pad=FALSE)
    cor_rel_diff <- as.data.frame(cbind(estimate=unlist(corr_diff[4,]), p_value=corr_diff[3,]))
    rel_diff_output <- apply(cbind(total_data[w_lb:sum(nrow(total_data)-w_ub),output_vars], cor_rel_diff), 2, as.character)
    output_list[["Relative Differences"]] <- rel_diff_output[complete.cases(rel_diff_output),]
    #plot(total_data$Age[5:sum(nrow(total_data)-4)], cor_rel_diff$estimate, ylab="Correlation", type="l", xlab="Year CE")
  }
  
  print("Adjusting p-values...")
  if(pval_corrections=="all") {
    pval_colnames <- c("Bonferroni p-value", "BH p-value", "Holm p-value", "Hochberg p-value", "Hommel p-value", "BY p-value")
    pval_methods <- c("bonferroni", "BH", "holm", "hochberg", "hommel", "BY")
  } else if(pval_corrections!="all") {
    if(any(!pval_corrections %in% c("bonferroni", "BH", "holm", "hochberg", "hommel", "BY"))) {
      stop("Incorrect p-value correction methods detected (pval_corrections)! These must be: bonferroni, BH, holm, hochberg, hommel and/or BY.")
    }
    
    pval_methods <- pval_corrections
    if(is.null(pval_labels) | pval_labels=="auto") {
      pval_colnames <- pval_corrections
      
      if(c("bonferroni") %in% pval_corrections) {
        pval_colnames <- gsub("bonferroni", "Bonferroni p-values", pval_colnames)
      }
      
      if(c("BH") %in% pval_corrections) {
        pval_colnames <- gsub("BH", "BH p-values", pval_colnames)
      }
      
      if(c("holm") %in% pval_corrections) {
        pval_colnames <- gsub("holm", "Holm p-values", pval_colnames)
      }
      
      if(c("hochberg") %in% pval_corrections) {
        pval_colnames <- gsub("hochberg", "Hochberg p-values", pval_colnames)
      }
      
      if(c("hommel") %in% pval_corrections) {
        pval_colnames <- gsub("hommel", "Hommel p-values", pval_colnames)
      }
      
      if(c("BY") %in% pval_corrections) {
        pval_colnames <- gsub("BY", "BY p-values", pval_colnames)
      }
    } else if(!is.null(pval_labels) & pval_labels!="auto") {
      pval_colnames <- pval_labels
    }
  }
  
  for(i in names(output_list)) {
    for(j in 1:length(pval_methods)) {
      list_item <- as.data.frame(apply(as.data.frame(output_list[[i]]),2, as.numeric))
      list_item <- list_item[order(list_item[,"p_value"]),]
      list_item[,pval_colnames[j]] <- p.adjust(as.numeric(list_item[,"p_value"]), method=pval_methods[j])
      list_item <- list_item[order(list_item[,x_var]),]
      output_list[[i]] <- list_item
    }
  }
  
  # Setting up plotting data, starting with the variables and labels
  pval_total_names <- c("p_value", pval_colnames)
  pval_total_labels <- c("Original p-value", pval_colnames)
  var_subset <- c("estimate")
  
  if(cor_method=="pearson") {
    var_labels <- c("Correlation (Pearson's r)")
  } else if(cor_method=="kendall") {
    var_labels <- c("Kendall's tau")
  } else if(cor_method=="spearman") {
    var_labels <- c("Spearman's rho")
  }
  
  print("Calculating changepoints...")
  if(any(!chgpt_methods %in% c("Agglomerative", "Divisive", "CP3O", "KS CP3O", "Univar")) & chgpt_methods!="all") {
    stop("Incorrect changepoint method selection(s) detected! These must be: Agglomerative, Divisive, CP3O, KS CP3O and/or Univar.")
  }
  
  if(c("Agglomerative") %in% chgpt_methods | chgpt_methods=="all") {
    ECP_list_agglo <- list()
  }
  
  if(c("Divisive") %in% chgpt_methods | chgpt_methods=="all") {
    ECP_list_div <- list()
  }
  
  if(c("CP3O") %in% chgpt_methods | chgpt_methods=="all") {
    ECP_list_cp3o <- list()
  }
  
  if(c("KS CP3O") %in% chgpt_methods | chgpt_methods=="all") {
    ECP_list_kscp3o <- list()
  }
  
  if(c("Univar") %in% chgpt_methods | chgpt_methods=="all") {
    CP_list <- list()
  }
  
  for(i in names(output_list)) {
    
    chngpt_data <- output_list[[i]][,var_subset]
    
    if(c("Agglomerative") %in% chgpt_methods | chgpt_methods=="all") {
      agglo_obj <- e.agglo(as.matrix(chngpt_data), member=1:length(chngpt_data), alpha=1)
      ECP_list_agglo[[i]] <- as.integer(agglo_obj$estimates)
    }
    
    if(c("Divisive") %in% chgpt_methods | chgpt_methods=="all") {
      div_obj <- e.divisive(as.matrix(chngpt_data), sig.lvl=0.05, R=200, k=NULL, min.size = 6, alpha=1)
      ECP_list_div[[i]] <- as.integer(div_obj$estimates)
    }
    
    if(c("CP3O") %in% chgpt_methods | chgpt_methods=="all") {
      cp3o_obj <- e.cp3o(as.matrix(chngpt_data), K=10, minsize = 6, alpha = 1)
      ECP_list_cp3o[[i]] <- as.integer(cp3o_obj$estimates)
    }
    
    if(c("KS CP3O") %in% chgpt_methods | chgpt_methods=="all") {
      ks_obj <- ks.cp3o(as.matrix(chngpt_data), K=10, minsize = 6)
      ECP_list_kscp3o[[i]] <- as.integer(ks_obj$estimates)
    }
    
    if(c("Univar") %in% chgpt_methods | chgpt_methods=="all") {
      univar_CP_obj <- cpt.meanvar(t(as.matrix(chngpt_data)), penalty="MBIC", pen.value=0,
                                   method="PELT", class=FALSE, param.estimates = TRUE, minseglen=6,
                                   test.stat = "Normal") #Q=6 number of cpts/segments to lookf for. Only for SegNeigh and BinSeg methods!
      CP_list[[i]] <- as.integer(univar_CP_obj[[1]])
    }
  }
  
  Total_CP_list <- list()
  
  if(c("Agglomerative") %in% chgpt_methods | chgpt_methods=="all") {
    Total_CP_list[["Agglomerative ECP"]] <- ECP_list_agglo
  }
  
  if(c("Divisive") %in% chgpt_methods | chgpt_methods=="all") {
    Total_CP_list[["Divisive ECP"]] <- ECP_list_div
  }
  
  if(c("CP3O") %in% chgpt_methods | chgpt_methods=="all") {
    Total_CP_list[["CP3O"]] <- ECP_list_cp3o
  }
  
  if(c("KS CP3O") %in% chgpt_methods | chgpt_methods=="all") {
    Total_CP_list[["KS CP3O"]] <- ECP_list_kscp3o
  }
  
  if(c("Univar") %in% chgpt_methods | chgpt_methods=="all") {
    Total_CP_list[["Univariate CP"]] <- CP_list
  }
  
  print("Generating plots...")
  CP_Plot_List <- list()
  changepoint_results <- list()
  runcor_results <- list()
  
  for(i in names(Total_CP_list)) {
    for(j in names(output_list)) {
      for(k in 1:length(pval_total_names)) {
        chngpt_plot_data <- output_list[[j]]
        p_val_var <- pval_total_names[k]
        plot_data <- as.data.frame(chngpt_plot_data[,c(x_var, var_subset, p_val_var)])
        match_cpts <- as.integer(Total_CP_list[[i]][[j]])
        plot_data[,"p_ranges"] <- cut(plot_data[,p_val_var], c(-Inf, 0.001, 0.05, Inf))
        plot_data[,"p_ranges"] <- factor(plot_data[,"p_ranges"], levels=c("(0.05, Inf]","(0.001,0.05]","(-Inf,0.001]"))
        plotname <- paste("Changepoints for ",j," (", i, " and ", pval_total_labels[k],")", sep="")
        y_var <- var_subset
        cpt_plot_data <- cpt_output_data <- as.data.frame(plot_data[c(match_cpts), x_var])
        colnames(cpt_plot_data) <- x_var
        
        output_name <- paste("Changepoints for ",j," (", i, ")", sep="")
        colnames(cpt_output_data) <- paste(j, " (", i, ")", sep="")
        changepoint_results[[output_name]] <- cpt_output_data
        runcor_results[[plotname]] <- plot_data
        
        CP_Plot_List[[plotname]] <- ggplot(data=plot_data, aes_string(x=x_var, y=y_var, 
                                                                      colour="p_ranges", 
                                                                      fill="p_ranges",
                                                                      shape="p_ranges")) +
          geom_path(data=plot_data[order(plot_data[,x_var]),], aes(group=1), colour=plot_cols[4], size=0.6) +
          geom_vline(data=cpt_plot_data, aes_string(xintercept=x_var), colour=plot_cols[3], alpha=1, linetype=2) +
          geom_point(colour="black", size=2.5) +
          scale_colour_manual(values=plot_cols) +
          scale_fill_manual(values=plot_cols, name=NULL, labels=c("<95%", ">95%", ">99%")) +
          scale_shape_manual(values=c(21,22,23), name=NULL, labels=c("<95%", ">95%", ">99%")) +
          #scale_colour_manual(name=p_val_var,
          #values=c("(-Inf, 0.001]"=colours[1],
          #"(0.001, 0.05]"=colours[2],
          # "(0.05, Inf]"=colours[3]),
          # labels=c("<95%", ">95%", ">99%")) +
          theme(#aspect.ratio=1,
            plot.title=element_text(size=11),
            axis.text.x = element_text(colour="black", angle = 0, size=10, margin=ggplot2::margin(c(0.1,0.1,0.1,0.1), unit="cm")),
            axis.text.y = element_text(colour="black", size=10, margin=ggplot2::margin(c(0.1,0.1,0.1,0.1), unit="cm")),
            axis.ticks.length=unit(0.1, "cm"),
            #axis.ticks=element_line(),
            plot.margin = grid::unit(c(1, 1, 1, 1), "lines"),
            axis.line=element_line(colour="grey75"),
            panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.major.y=element_line(colour="grey70",linetype=3),
            #panel.grid.major=element_blank(),
            panel.border=element_rect(fill=NA, colour="grey40"),
            panel.background=element_blank(),
            axis.title.x=element_text(colour="black", margin=ggplot2::margin(t=0.1, unit="cm")),
            axis.title.y=element_text(colour="black", margin=ggplot2::margin(r=0.1, unit="cm")),
            legend.position="bottom", legend.direction="horizontal",
            legend.key=element_blank(),
            legend.margin=ggplot2::margin(t=0, unit="cm"),
            legend.text=element_text(size=11),
            legend.text.align=0,
            legend.title=element_text(face="bold")) +
          labs(x=x_lab, y=var_labels, title=plotname)
        
        if(!any(y_limit=="none") & !any(is.na(y_limit))) {
          CP_Plot_List[[plotname]] <-  CP_Plot_List[[plotname]] + coord_cartesian(ylim = y_limit) 
        }
        
      }
    }
  }
  
  if(export_results==TRUE) {
    print("Exporting relevant results as an .xlsx file...")
    RCor_Results <- createWorkbook()
    
    for(i in names(output_list)) {
    sheet1 <- createSheet(RCor_Results, paste(i))
    addDataFrame(as.data.frame(output_list[[i]]), sheet=sheet1, startColumn=1, startRow=1, row.names=FALSE)
    #addDataFrame("Percentage confusion matrix (accuracy across CV resamples)", sheet=sheet1, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE)
    #addDataFrame("Reference", sheet=sheet1, startRow = 2, startColumn = 2, col.names = FALSE, row.names = FALSE)
    #addDataFrame("Prediction", sheet=sheet1, startRow = 3, startColumn = 1, col.names = FALSE, row.names = FALSE)
    }
    
    sheet2 <- createSheet(RCor_Results, "Changepoint Results")
    rcor_res <- as.data.frame(do.call(qpcR:::cbind.na, changepoint_results))
    rcor_res[is.na(rcor_res)] <- ""
    addDataFrame(rcor_res, sheet=sheet2, startColumn=1, startRow=1, row.names=FALSE)
    
    file_name <- gsub("\\.csv$", "", main_file)
    saveWorkbook(RCor_Results, paste(export_path, "/RunCor_", file_name, "_", cor_vars[1], " and ", cor_vars[2], "_", format(Sys.time(), "%Y-%m-%d %I-%p"), ".xlsx", sep=""))
  }
  
    
  if(print_plots==TRUE) {
    print("Printing plots...")
  print(CP_Plot_List)
  }
  
  if(export_plots==TRUE) {
    print("Exporting plots as .PDF files...")
    pdf(paste(export_path, "/RunCor Plots for ", file_name, "_", cor_vars[1], " and ", cor_vars[2], " ", format(Sys.time(), "%Y-%m-%d %I-%p") , ".pdf", sep=""),
        width=width, height=height)
    print(CP_Plot_List)
    dev.off()
  }
  print("Processing complete!")
  return(NULL)
  #return(list(output_list, CP_Plot_List, runcor_results, changepoint_results))
}
