wind_data <- readr::read_rds("Data/data.rds")

month_names <- c("March", "April", "May")

obs_test <- list()

for (i in seq(3)) {
  obs_test[[i]] <- tail(wind_data[[i]]$Original, 1008)
  names(obs_test)[i] <- month_names[i]
}

files_list <- list.files(here::here("new-results"))

new_metrics <- matrix(nrow = length(obs_test[[1]]), ncol = 12*3)
colnames(new_metrics) <-
  files_list |>
  stringr::str_remove("[a-zA-Z]+\\.[a-zA-Z]+$") |>
  stringr::str_remove("Original_")

for (file in seq(files_list)) {
  print(files_list[file])

  y_hat <-
    read.csv(here::here("new-results", files_list[file])) |>
    dplyr::pull(y_hat)

  if (file %in% c(1, 2, 3, 10, 11, 12, 19, 20, 21, 28, 29, 30)) {
    erro <- obs_test[["April"]] - y_hat
  } else if (file %in% c(4,5,6,13,14,15,22,23,24,31,32,33)) {
    erro <- obs_test[["March"]] - y_hat
  } else {
    erro <- obs_test[["May"]] - y_hat
  }

  new_metrics[, file] <- c(erro)
}

new_metrics |>
  write.csv(here::here("Results", "new_error.csv"))

steps <- c(1,3,6)

error_ <- list()

for (month in month_names) {
  error_[[month]] <- list()
  for (step_ in seq(3)) {
    step <- steps[step_]

    comb <- paste(month, step, sep = "_")

    error_[[month]][[step_]] <-
      new_metrics |>
      dplyr::as_tibble() |>
      dplyr::mutate(n = seq(dplyr::n())) |>
      tidyr::pivot_longer(cols = -n) |>
      dplyr::filter(grepl(comb, name)) |>
      dplyr::arrange(name, n) |>
      tidyr::pivot_wider(id_cols = n)

    colnames(error_[[month]][[step_]]) <- c("n", "V", "W", "X", "F")
  }
  names(error_[[month]]) <- paste0("step_", steps)
}


final_error <- list()
for (month in month_names) {
  final_error[[month]] <- list()
  for (step_ in seq(3)) {
    final_error[[month]][[step_]] <-
      error[[month]][[step_]] |>
      dplyr::as_tibble() |>
      dplyr::mutate(n = seq(dplyr::n())) |>
      dplyr::select(n, A) |>
      dplyr::left_join(error_[[month]][[step_]], by = "n") |>
      dplyr::select(-n)
  }
  names(final_error[[month]]) <- paste0("step_", steps)
}

## DM test ----
DM_tvalue <- list()
DM_pvalue <- list()
DM_presult <- list()

horizons <- c(1,3,6)

for (dataset in seq(3)) {
  DM_tvalue[[dataset]] <- list()
  DM_pvalue[[dataset]] <- list()
  DM_presult[[dataset]] <- list()
  for (horizon in seq(3)) {
    # DM test
    DM_tvalue[[dataset]][[horizon]] <- matrix(
      nrow = ncol(final_error[[dataset]][[horizon]]),
      ncol = 1
    )
    colnames(DM_tvalue[[dataset]][[horizon]]) <- paste0(horizons[horizon], "0 min")
    rownames(DM_tvalue[[dataset]][[horizon]]) <- colnames(final_error[[dataset]][[horizon]])
    DM_pvalue[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]
    DM_presult[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]

    for (col in seq(ncol(DM_tvalue[[dataset]][[horizon]]))) {
      for (row in seq(nrow(DM_tvalue[[dataset]][[horizon]]))) {
        if (col == row) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else if (final_error[[dataset]][[horizon]][,col] == final_error[[dataset]][[horizon]][,row]) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else {
          DMtest <- forecast::dm.test(
            final_error[[dataset]][[horizon]][,col] |> dplyr::pull(),
            final_error[[dataset]][[horizon]][,row] |> dplyr::pull(),
            h = horizon, power = 1
          )
          DM_tvalue[[dataset]][[horizon]][row,col] <- DMtest$statistic
          DM_pvalue[[dataset]][[horizon]][row,col] <- DMtest$p.value

          if (DM_pvalue[[dataset]][[horizon]][row,col] <= 0.01) {
            DM_presult[[dataset]][[horizon]][row,col] <- '*'
          } else if (DM_pvalue[[dataset]][[horizon]][row,col] <= 0.05 && DM_pvalue[[dataset]][[horizon]][row,col] > 0.01){
            DM_presult[[dataset]][[horizon]][row,col] <- '**'
          } else if (DM_pvalue[[dataset]][[horizon]][row,col] > 0.05){
            DM_presult[[dataset]][[horizon]][row,col] <- NA
          }
        }
      }
    }

  }
  names(DM_tvalue[[dataset]]) <- names(final_error[[dataset]])
  names(DM_pvalue[[dataset]]) <- names(final_error[[dataset]])
  names(DM_presult[[dataset]]) <- names(final_error[[dataset]])
}

names(DM_tvalue) <- names(final_error)
names(DM_pvalue) <- names(final_error)
names(DM_presult) <- names(final_error)

tvalue <- do.call(cbind, do.call(cbind, DM_tvalue))
pvalue <- do.call(cbind, do.call(cbind, DM_pvalue))
presult <- do.call(cbind, do.call(cbind, DM_presult))

tvalue %>%
  write.csv(here::here("new_ip", "DM_tvalue.csv"),
            row.names = T,
            col.names = T)

pvalue %>%
  write.csv(here::here("new_ip", "DM_pvalue.csv"),
            row.names = T,
            col.names = T)

presult %>%
  write.csv(here::here("new_ip", "DM_presult.csv"),
            row.names = T,
            col.names = T)

final_result <- matrix(ncol = ncol(tvalue), nrow = nrow(tvalue))
colnames(final_result) <- colnames(tvalue)
rownames(final_result) <- rownames(tvalue)

for (col in seq(ncol(tvalue))) {
  for (row in seq(nrow(tvalue))) {
    final_result[row, col] <-
      paste0(
        sprintf("%4.2f", tvalue[row, col]),
        presult[row, col]
      )
  }
}

final_result |>
xtable::xtable()
