wind_data <- readr::read_rds("Data/data.rds")

month_names <- c("March", "April", "May")

obs_test <- list()

for (i in seq(3)) {
  obs_test[[i]] <- tail(wind_data[[i]]$Original, 1008)
  names(obs_test)[i] <- month_names[i]
}

files_list <- list.files(here::here("new-results"))

new_metrics <- matrix(nrow = 12*3, ncol = 6)
colnames(new_metrics) <- c("mae", "mape", "rmse", "rrmse", "sse", "std")
rownames(new_metrics) <-
  files_list |>
  stringr::str_remove("[a-zA-Z]+\\.[a-zA-Z]+$") |>
  stringr::str_remove("Original_")

for (file in seq(files_list)) {
  print(files_list[file])

  y_hat <-
    read.csv(here::here("new-results", files_list[file])) |>
    dplyr::pull(y_hat)

  if (file %in% c(1, 2, 3, 10, 11, 12, 19, 20, 21, 28, 29, 30)) {
    mae <- Metrics::mae(y_hat, obs_test[["April"]])
    mape <- Metrics::mape(y_hat, obs_test[["April"]])
    rmse <- Metrics::rmse(y_hat, obs_test[["April"]])
    rrmse <- Metrics::rmse(y_hat, obs_test[["April"]]) / mean(y_hat)
    sse <- Metrics::sse(y_hat, obs_test[["April"]])
    std <- sd((obs_test[["April"]] - y_hat))
  } else if (file %in% c(4,5,6,13,14,15,22,23,24,31,32,33)) {
    mae <- Metrics::mae(y_hat, obs_test[["March"]])
    mape <- Metrics::mape(y_hat, obs_test[["March"]])
    rmse <- Metrics::rmse(y_hat, obs_test[["March"]])
    rrmse <- Metrics::rmse(y_hat, obs_test[["March"]]) / mean(y_hat)
    sse <- Metrics::sse(y_hat, obs_test[["March"]])
    std <- sd((obs_test[["March"]] - y_hat))
  } else {
    mae <- Metrics::mae(y_hat, obs_test[["May"]])
    mape <- Metrics::mape(y_hat, obs_test[["May"]])
    rmse <- Metrics::rmse(y_hat, obs_test[["May"]])
    rrmse <- Metrics::rmse(y_hat, obs_test[["May"]]) / mean(y_hat)
    sse <- Metrics::sse(y_hat, obs_test[["May"]])
    std <- sd((obs_test[["May"]] - y_hat))
  }

  new_metrics[file, ] <- c(mae, mape, rmse, rrmse, sse, std)
}

new_metrics |>
  write.csv(here::here("Results", "new_metrics.csv"))
