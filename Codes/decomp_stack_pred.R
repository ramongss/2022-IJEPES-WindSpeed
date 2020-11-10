decomp_stack_pred <- function(data, model_list, meta_model, horizon){
  cat('\n\n######### Decomposition Prediction #########\n###########',
      as.character(format.Date(data$TimeStamp[1])),'###########\n\n')
  # Data preprocessing ------------------------------------------------------
  # create dataframes
  lag <- 5 # number of lags

  IMF <- list() # list to save dfs
  
  # create df for each imf with lags and inputs
  for (ii in seq(5)) {
    IMF[[ii]] <- data.frame(
      lags(data[,ii+1], lag)
    )
    
    # rename columns
    names(IMF[[ii]]) <- c(
      'y',
      paste0('lag',seq(lag))
    )
  }
  
  # rename the list
  names(IMF) <- paste0('c', seq(5))
  
  # create training and test sets
  IMF_train  <- list()
  IMF_test   <- list()
  IMF_xtrain <- list()
  IMF_ytrain <- list()
  IMF_xtest  <- list()
  IMF_ytest  <- list()
  
  for (i in seq(IMF)) {
    n <- dim(IMF[[i]])[1]
    cut <- round(n * 0.8)
    
    IMF_train[[i]] <- IMF[[i]][1:cut,]
    IMF_test[[i]]  <- tail(IMF[[i]],n-cut)
    
    IMF_xtrain[[i]] <- IMF_train[[i]][,-1]
    IMF_ytrain[[i]] <- IMF_train[[i]][,1]
    
    IMF_xtest[[i]] <- IMF_test[[i]][,-1]
    IMF_ytest[[i]] <- IMF_test[[i]][,1]
  }
  
  # create Observed dfs
  Obs <- lags(data$Original, lag)[,1]
  Obs_train <- Obs[1:cut]
  Obs_test  <- tail(Obs,n-cut)
  
  IMF_df <- data.frame(data$Original, data[,2:6])
  colnames(IMF_df) <- c('Obs', paste0('c', seq(5)))
  
  # Training phase ----------------------------------------------------------
  # set random seed
  set.seed(1406)

  # set traincontrol
  control <- trainControl(
    method = "cv",
    number = 5
  )
  
  # list of training models
  model_list <- model_list
  
  # define objects
  IMF_model <- list()
  IMF_pred <- list()
  Params <- list()
  Importance <- list()
  k <- 1 # aux
  
  # training and predicting each IMF with each model
  for (imf in seq(IMF)) {
    Importance[[imf]] <- matrix(nrow = ncol(IMF_xtrain[[imf]]), ncol = length(model_list))
    colnames(Importance[[imf]]) <- model_list
    rownames(Importance[[imf]]) <- colnames(IMF_xtrain[[imf]])
    for (model in seq(model_list)) {
      # fitting
      IMF_model[[k]] <- train(
        y~., data = IMF_train[[imf]],
        method = model_list[model],
        trControl = control,
        preProcess = c('center','scale'),
        tuneLength = 5,
        trace = FALSE
      )
      
      # save hyperparameters
      Params[[k]] <- IMF_model[[k]]$bestTune
      
      # save variables importance
      Importance[[imf]][,model] <- varImp(IMF_model[[k]], 
                                          scale = FALSE)$importance$Overall
      
      # prediction
      IMF_train_pred <- predict(IMF_model[[k]], IMF_train[[imf]])
      IMF_test_pred <- predict(IMF_model[[k]], IMF_test[[imf]])
      IMF_pred[[k]] <- data.frame(c(IMF_train_pred, IMF_test_pred))
      
      # print steps
      cat("\nModel: ", model_list[model], "\tc", imf, "\t",
          as.character(format.Date(Sys.time(), '%H:%M:%S')),
          sep = '')
      
      # update k aux
      k <- k + 1
    }
  }
  
  names(Importance) <- names(IMF)
  
  # create a matrix combination
  combs <- matrix(nrow = length(model_list), ncol = length(IMF))
  colnames(combs) <- names(IMF)
  
  for (i in seq(model_list)) {
    for (j in seq(IMF)) {
      combs[i,j] <- i
    }
  }
  
  # Multi-step predictions ----
  
  # save IMF_model into a matrix
  model_matrix <- data.frame(matrix(IMF_model, ncol=length(IMF), byrow = FALSE))
  names(model_matrix) <- names(IMF)
  
  ## Recursive prediction
  
  # define objects
  step_vmd_pred <- list()
  metrics_vmd_train <- list()
  metrics_vmd_test <- list()
  stack_model <- list()
  stack_params <- list()
  stack_pred <- list()
  metrics_stack_train <- list()
  metrics_stack_test <- list()
  predictions <- list()
  errors <- list()
  horizon <- horizon
  for (h in seq(length(horizon))) {
    {
      hrz <- horizon[h]
      PTRmo <- list()
      PTEmo <- list()
      Comp_train <- list()
      Comp_test <- list()
      k <- 1
    }
    
    cat('\nHorizon: ', hrz, 'steps\n')
    
    for (m in 1:length(model_list)) {
      
      Comp_train[[m]] <- IMF_xtrain
      
      Comp_test[[m]] <- IMF_xtest
      
      PTRmo[[m]] <- matrix(ncol = length(IMF), nrow = nrow(IMF_xtrain[[1]]))
      PTEmo[[m]] <- matrix(ncol = length(IMF), nrow = nrow(IMF_xtest[[1]]))
      
      for (c in 1:length(Comp_train[[m]])) {
        if (h == 1) {
          # train
          PTRmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]]))
          
          # test
          PTEmo[[m]][,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]]))
        } else {
          # train
          for(p in 1:cut) {
            if(p%%hrz !=1) {
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(h-1)) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              }
            } else {
              Comp_train[[m]][[c]][p:cut,] <- IMF_xtrain[[c]][p:cut,]
              PTRmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_train[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(h-1)) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_train[[m]][[c]][p+l,l] <- PTRmo[[m]][p,c]}
              }
            }
          }
          
          # test
          for(p in 1:(n-cut)) {
            if(p%%hrz !=1) {
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(h-1)) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              }
            } else {
              Comp_test[[m]][[c]][p:(n-cut),] <- IMF_xtest[[c]][p:(n-cut),]
              PTEmo[[m]][p,c] <- (predict(model_matrix[[m,c]], Comp_test[[m]][[c]][p,]))
              if (hrz <= lag){
                for (l in 1:(h-1)) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              } else {
                for (l in 1:lag) {Comp_test[[m]][[c]][p+l,l] <- PTEmo[[m]][p,c]}
              }
            }
          }
        }
        
        cat("Model: ", model_list[m], "\tc: ", c , "\t", 
            (k/(length(model_list)*length(Comp_train[[m]])))*100,"%\n", sep = "")
        
        k <- k + 1
      }
    }
    
    {
      metrics_vmd_train[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      metrics_vmd_test[[h]] <- matrix(nrow = dim(combs)[1],ncol = 4)
      colnames(metrics_vmd_train[[h]]) <- c("i","MAE","MAPE","RMSE")
      colnames(metrics_vmd_test[[h]]) <- colnames(metrics_vmd_train[[h]])
      rownames(metrics_vmd_train[[h]]) <- model_list
      rownames(metrics_vmd_test[[h]]) <- rownames(metrics_vmd_train[[h]])
      
      step_vmd_pred_train <- matrix(nrow = dim(IMF_xtrain[[1]])[1], ncol = dim(combs)[1])
      step_vmd_pred_test <- matrix(nrow = dim(IMF_xtest[[1]])[1], ncol = dim(combs)[1])
      step_vmd_pred[[h]] <- matrix(nrow = n, ncol = dim(combs)[1])
    }
    
    for (i in 1:dim(combs)[1]) {
      step_vmd_pred_train[,i] <- rowSums(PTRmo[[i]])
      step_vmd_pred_test[,i] <- rowSums(PTEmo[[i]])
      
      ### Avoiding negative values
      for (j in 1:dim(step_vmd_pred_train)[1]) {
        if (step_vmd_pred_train[j,i] < 0) {
          step_vmd_pred_train[j,i] <- 0
        }
      }
      for (j in 1:dim(step_vmd_pred_test)[1]) {
        if (step_vmd_pred_test[j,i] < 0) {
          step_vmd_pred_test[j,i] <- 0
        }
      }
      
      step_vmd_pred[[h]][,i] <- c(step_vmd_pred_train[,i],
                                  step_vmd_pred_test[,i])
      
      # metrics
      vmd_step_mae_train <- MAE(step_vmd_pred_train[,i], Obs_train)
      vmd_step_mape_train <- mape(step_vmd_pred_train[,i], Obs_train)
      vmd_step_rmse_train <- RMSE(step_vmd_pred_train[,i], Obs_train)
      
      vmd_step_mae_test <- MAE(step_vmd_pred_test[,i], Obs_test)
      vmd_step_mape_test <- mape(step_vmd_pred_test[,i], Obs_test)
      vmd_step_rmse_test <- RMSE(step_vmd_pred_test[,i], Obs_test)
      
      metrics_vmd_train[[h]][i,] <- c(i,
                                      vmd_step_mae_train,
                                      vmd_step_mape_train,
                                      vmd_step_rmse_train)
      metrics_vmd_test[[h]][i,] <- c(i,
                                     vmd_step_mae_test,
                                     vmd_step_mape_test,
                                     vmd_step_rmse_test)
    }
    step_vmd_pred[[h]] <- cbind(Obs, step_vmd_pred[[h]]) 
    colnames(step_vmd_pred[[h]]) <- c('Obs',model_list)
    
    # Stacking ----------------------------------------------------------------
    cat("\nStacking\n")
    stack_data <- step_vmd_pred[[h]]
    colnames(stack_data) <- c("y", model_list)
    
    stack_data_train <- stack_data[1:cut,]
    stack_data_test <- tail(stack_data,n-cut)
    
    meta_model <- meta_model
    
    stack_pred_train <- matrix(nrow = nrow(stack_data_train), ncol = 1)
    stack_pred_test <- matrix(nrow = nrow(stack_data_test), ncol = 1)
    stack_pred[[h]] <- matrix(nrow = n, ncol = 1)
    
    metrics_stack_train[[h]] <- matrix(nrow = 1, ncol = 3)
    metrics_stack_test[[h]] <- matrix(nrow = 1, ncol = 3)
    colnames(metrics_stack_train[[h]]) <- c("MAE","MAPE","RMSE")
    colnames(metrics_stack_test[[h]]) <- colnames(metrics_stack_train[[h]])

    stack_model[[h]] <- train(
      y~., data = stack_data_train,
      method = meta_model,
      trControl = control,
      preProcess = c('center','scale'),
      trace = FALSE
    )
    
    # save hyperparameters
    stack_params[[h]] <- stack_model[[h]]$bestTune

    # prediction
    stack_train_pred <- predict(stack_model[[h]], stack_data_train)
    stack_test_pred <- predict(stack_model[[h]], stack_data_test)
    stack_pred[[h]] <- c(stack_train_pred, stack_test_pred)

    # metrics
    stack_mae_train <- MAE(stack_train_pred, Obs_train)
    stack_mape_train <- mape(stack_train_pred, Obs_train)
    stack_rmse_train <- RMSE(stack_train_pred, Obs_train)

    stack_mae_test <- MAE(stack_test_pred, Obs_test)
    stack_mape_test <- mape(stack_test_pred, Obs_test)
    stack_rmse_test <- RMSE(stack_test_pred, Obs_test)

    metrics_stack_train[[h]] <- c(stack_mae_train,
                                  stack_mape_train,
                                  stack_rmse_train)
    metrics_stack_test[[h]] <- c(stack_mae_test,
                                 stack_mape_test,
                                 stack_rmse_test)
  
    predictions[[h]] <- cbind(Obs, stack_pred[[h]], step_vmd_pred[[h]][,-1])
    
    errors[[h]] <- matrix(ncol = ncol(predictions[[h]])-1, nrow = n)
    colnames(errors[[h]]) <- colnames(predictions[[h]][,-1])
    
    for (error in seq(ncol(errors[[h]]))) {
      errors[[h]][,error] <- (predictions[[h]][,1] - predictions[[h]][,1+error])
    }
    
  }
  names(step_vmd_pred) <- paste0(horizon,'-step')
  names(metrics_vmd_train) <- names(step_vmd_pred)
  names(metrics_vmd_test) <- names(step_vmd_pred)
  names(metrics_stack_train) <- names(step_vmd_pred)
  names(metrics_stack_test) <- names(step_vmd_pred)
  names(stack_pred) <- names(step_vmd_pred)
  names(predictions) <- names(step_vmd_pred)
  names(errors) <- names(step_vmd_pred)
  
  results <- list(Predictions = predictions,
                  VMD_Metrics = metrics_vmd_test,
                  STACK_Metrics = metrics_stack_test,
                  Hyperparameters = Params,
                  Stack_params = stack_params,
                  Var_Importance = Importance,
                  Errors = errors,
                  IMF = IMF_df)
  
  return(results)
}