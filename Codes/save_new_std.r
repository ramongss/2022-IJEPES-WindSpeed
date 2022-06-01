std <-
  readr::read_csv(here::here("Results", "new_metrics.csv")) |>
  dplyr::select(models = ...1, std)


readr::read_csv(here::here("Data", "dataset3_std.csv")) |>
dplyr::mutate(step = 1:3) |>
tidyr::pivot_longer(cols = LETTERS[1:24]) |>
dplyr::filter(!is.na(value)) |>
dplyr::full_join(
  std |>
  dplyr::filter(grepl("LSTM_VMD_SSA_May", models)) |>
  dplyr::select(F = std) |>
  dplyr::mutate(step = 1:3) |>
  tidyr::pivot_longer(cols = "F")
) |>
dplyr::full_join(
  std |>
  dplyr::filter(grepl("ANN_May", models)) |>
  dplyr::select(V = std) |>
  dplyr::mutate(step = 1:3) |>
  tidyr::pivot_longer(cols = "V")
) |>
dplyr::full_join(
  std |>
  dplyr::filter(grepl("ESN_May", models)) |>
  dplyr::select(W = std) |>
  dplyr::mutate(step = 1:3) |>
  tidyr::pivot_longer(cols = "W")
) |>
dplyr::full_join(
  std |>
  dplyr::filter(grepl("LSTM_May", models)) |>
  dplyr::select(X = std) |>
  dplyr::mutate(step = 1:3) |>
  tidyr::pivot_longer(cols = "X")
) |>
dplyr::arrange(step, name) |>
tidyr::pivot_wider(
  id_cols = step,
  names_from = name,
  values_from = value
) |>
dplyr::select(-step) |>
write.csv("Data/dataset3_std.csv", row.names = FALSE)
