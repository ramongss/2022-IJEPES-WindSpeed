stack_pred <- function(data, model_list, meta_model, horizon){
  cat('\n\n######### Single Prediction #########\n###########',
      as.character(format.Date(data$TimeStamp[1])),'###########\n\n')
  # Data Preprocessing ----
  # number of lags
  lag <- 5
  
  # create lags and inputs
  data_lag <- data.frame(
    lags(data$Original, lag)
  )
  # rename columns
  colnames(data_lag) <- c('y',paste0('lag', seq(lag)))
  
  # create train and test sets
  n <- nrow(data_lag)
  cut <- round(n * 0.8)
  
  train <- data_lag[1:cut,]
  test <- tail(data_lag, n-cut)
  
  x_train <- train[,-1]
  y_train <- train[,1]
  
  x_test <- test[,-1]
  y_test <- test[,1]
  
  # create Obs
  Obs <- data_lag$y
  Obs_train <- Obs[1:cut]
  Obs_test  <- tail(Obs,n-cut)
  
  # Training phase ----
  # set random seed
  set.seed(1406)
  
  # set trainControl
  control <- trainControl(
    method = "cv",
    number = 5
  )
  
  # list of training models
  model_list <- model_list
  
  # define objects
  models <- list()
  Params <- list()
  Importance <- matrix(nrow = ncol(x_train), ncol = length(model_list))
  colnames(Importance) <- model_list ; rownames(Importance) <- colnames(x_train)
  
  # training each model
  for (model in seq(model_list)) {
    # fitting
    models[[model]] <- train(
      y~., data = train,
      method = model_list[model],
      trControl = control,
      preProcess = c('center','scale'),
      tuneLength = 5,
      trace = FALSE
    )
    
    # save hyperparameters
    Params[[model]] <- models[[model]]$bestTune
    
    # save variables importance
    Importance[,model] <- varImp(models[[model]], scale = FALSE)$importance$Overall
    
    # print steps
    cat("\nModel: ", model_list[model], "\t",
        as.character(format.Date(Sys.time(), '%H:%M:%S')),
        sep = '')
  }
  
  # Multi-step predictions ----
  
  ## Recursive prediction
  
  # define objects
  PTRmo <- list()
  PTEmo <- list()
  single_step_pred <- list()
  metrics_train <- list()
  metrics_test <- list()
  stack_model <- list()
  stack_params <- list()
  stack_pred <- list()
  metrics_stack_train <- list()
  metrics_stack_test <- list()
  predictions <- list()
  errors <- list()
  horizon <- horizon
  
  for (h in seq(horizon)) {
    hrz <- horizon[h]
    PTRmo[[h]] <- matrix(ncol = length(model_list), nrow = nrow(train))
    PTEmo[[h]] <- matrix(ncol = length(model_list), nrow = nrow(test))
    metrics_train[[h]] <- matrix(nrow = length(model_list), ncol = 4)
    metrics_test[[h]] <- matrix(nrow = length(model_list), ncol = 4)
    colnames(metrics_train[[h]]) <- c("i","MAE","MAPE","RMSE")
    colnames(metrics_test[[h]]) <- colnames(metrics_train[[h]])
    rownames(metrics_train[[h]]) <- model_list
    rownames(metrics_test[[h]]) <- rownames(metrics_train[[h]])
    
    single_step_pred[[h]] <- matrix(nrow = n, ncol = length(model_list))
    colnames(single_step_pred[[h]]) <- model_list
    
    cat('\nHorizon: ', hrz, 'steps\n')
    
    for (m in seq(model_list)) {
      x_trainm <- as.data.frame(x_train)
      x_testm <- as.data.frame(x_test)
      
      if (h == 1) {
        # train
        PTRmo[[h]][,m] <- (predict(models[[m]], x_trainm))
        
        # test
        PTEmo[[h]][,m] <- (predict(models[[m]], x_testm))
      } else {
        # train
        for(p in seq(cut)) {
          if(p%%hrz != 1) {
            non_zero <- (predict(models[[m]], x_trainm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTRmo[[h]][p,m] <- non_zero
            if (hrz <= lag) {
              for (l in 1:(h-1)) {x_trainm[p+l,l] <- PTRmo[[h]][p,m]}
            } else {
              for (l in 1:lag) {x_trainm[p+l,l] <- PTRmo[[h]][p,m]}
            }
          } else {
            x_trainm[p:cut,] <- x_train[p:cut,]
            non_zero <- (predict(models[[m]], x_trainm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTRmo[[h]][p,m] <- non_zero
            if (hrz <= lag) {
              for (l in 1:(h-1)) {x_trainm[p+l,l] <- PTRmo[[h]][p,m]}
            } else {
              for (l in 1:lag) {x_trainm[p+l,l] <- PTRmo[[h]][p,m]}
            }
          }
        }
        
        # test
        for(p in seq(n-cut)) {
          if(p%%hrz !=1) {
            non_zero <- (predict(models[[m]], x_testm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTEmo[[h]][p,m] <- non_zero
            if (hrz <= lag) {
              for (l in 1:(h-1)) {x_testm[p+l,l] <- PTEmo[[h]][p,m]}
            } else {
              for (l in 1:lag) {x_testm[p+l,l] <- PTEmo[[h]][p,m]}
            }
          } else {
            x_testm[p:(n-cut),] <- x_test[p:(n-cut),]
            non_zero <- (predict(models[[m]], x_testm[p,]))
            if(non_zero < 0){non_zero <- 0}
            PTEmo[[h]][p,m] <- non_zero
            if (hrz <= lag) {
              for (l in 1:(h-1)) {x_testm[p+l,l] <- PTEmo[[h]][p,m]}
            } else {
              for (l in 1:lag) {x_testm[p+l,l] <- PTEmo[[h]][p,m]}
            }
          }
        }
      }
      
      # avoiding negative values
      for (j in seq(nrow(PTRmo[[h]]))) {
        if (PTRmo[[h]][j,m] < 0) {
          PTRmo[[h]][j,m] <- 0
        }
      }
      for (j in seq(nrow(PTEmo[[h]]))) {
        if (PTEmo[[h]][j,m] < 0) {
          PTEmo[[h]][j,m] <- 0
        }
      }
      
      single_step_pred[[h]][,m] <- c(PTRmo[[h]][,m], PTEmo[[h]][,m])
      
      # metrics
      step_mae_train <- MAE(PTRmo[[h]][,m], Obs_train)
      step_mape_train <- mape(PTRmo[[h]][,m], Obs_train)
      step_rmse_train <- RMSE(PTRmo[[h]][,m], Obs_train)
      
      step_mae_test <- MAE(PTEmo[[h]][,m], Obs_test)
      step_mape_test <- mape(PTEmo[[h]][,m], Obs_test)
      step_rmse_test <- RMSE(PTEmo[[h]][,m], Obs_test)
      
      
      metrics_train[[h]][m,] <- c(m,
                                  step_mae_train,
                                  step_mape_train,
                                  step_rmse_train)
      metrics_test[[h]][m,] <- c(m,
                                 step_mae_test,
                                 step_mape_test,
                                 step_rmse_test)
      
      
      cat("Model: ", model_list[m], "\t", 
          (m/(length(model_list)))*100,"%\n", sep = "")
    }
    
    # add Obs column
    single_step_pred[[h]] <- cbind(Obs, single_step_pred[[h]]) 
    colnames(single_step_pred[[h]]) <- c('Obs',model_list)
    
    # Stacking ----------------------------------------------------------------
    cat("\nStacking\n")
    stack_data <- single_step_pred[[h]]
    colnames(stack_data) <- c("y", model_list)
    
    stack_data_train <- stack_data[1:cut,]
    stack_data_test <- tail(stack_data,n-cut)
    
    meta_model <- meta_model
    
    stack_pred_train <- matrix(nrow = nrow(stack_data_train), ncol = 1)
    stack_pred_test <- matrix(nrow = nrow(stack_data_test), ncol = 1)
    stack_pred[[h]] <- matrix(nrow = n, ncol = 1)
    
    metrics_stack_train[[h]] <- matrix(nrow = 1, ncol = 4)
    metrics_stack_test[[h]] <- matrix(nrow = 1, ncol = 4)
    colnames(metrics_stack_train[[h]]) <- c("i","MAE","MAPE","RMSE")
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
      
    predictions[[h]] <- cbind(Obs, stack_pred[[h]], single_step_pred[[h]][,-1])
    
    # calculate errors
    errors[[h]] <- matrix(ncol = ncol(predictions[[h]])-1, nrow = n)
    colnames(errors[[h]]) <- colnames(predictions[[h]][,-1])
    for (error in seq(ncol(errors[[h]]))) {
      errors[[h]][,error] <- (predictions[[h]][,1] - predictions[[h]][,1+error])
    }
  }
  
  names(single_step_pred) <- paste0(horizon,'-step')
  names(metrics_train) <- names(single_step_pred)
  names(metrics_test) <- names(single_step_pred)
  names(metrics_stack_train) <- names(single_step_pred)
  names(metrics_stack_test) <- names(single_step_pred)
  names(stack_pred) <- names(single_step_pred)
  names(predictions) <- names(single_step_pred)
  names(errors) <- names(single_step_pred)
  
  results <- list(Predictions = predictions,
                  Metrics = metrics_test,
                  STACK_Metrics = metrics_stack_test,
                  Hyperparameters = Params,
                  Stack_params = stack_params,
                  Var_Importance = Importance,
                  Errors = errors)
  
  return(results)
}