metrics <- matrix(nrow = 9*3*5, ncol = 2)
colnames(metrics) <- c("rrmse", "sse")

counter <- 1
for (ds in names(decomp_stack_results)) {
  print(ds)
  for (step in names(decomp_stack_results[[ds]][["Predictions"]])) {
    print(step)
    for (model in seq(ncol(decomp_stack_results[[ds]][["Predictions"]][[step]][, -1]))) {
      print(model)

      model_name <- colnames(decomp_stack_results[[ds]][["Predictions"]][[step]][, -1])[model]

      obs <- tail(decomp_stack_results[[ds]][["Predictions"]][[step]][, 1], 1008)
      y_hat <- tail(decomp_stack_results[[ds]][["Predictions"]][[step]][, model+1], 1008)

      rrmse <- Metrics::rmse(y_hat, obs) / mean(y_hat)
      sse <- Metrics::sse(y_hat, obs)

      metrics[counter, ] <- c(rrmse, sse)

      counter <- counter + 1
    }
  }
}

row_names <- c()
counter <- 1
for (ds in names(decomp_stack_results)) {
  print(ds)
  for (step in names(decomp_stack_results[[ds]][["Predictions"]])) {
    print(step)
    for (model in seq(ncol(decomp_stack_results[[ds]][["Predictions"]][[step]][, -1]))) {
      print(model)
      model_name <- colnames(decomp_stack_results[[ds]][["Predictions"]][[step]][, -1])[model]
      i <- paste0(ds, "_", step, "_", model_name)
      row_names <- c(row_names, i)

      counter <- counter + 1
    }
  }
}

rownames(metrics) <- row_names

metrics |>
write.csv("new_old_metrics.csv")
