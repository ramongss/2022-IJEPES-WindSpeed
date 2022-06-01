wind_data <- readr::read_rds("data.rds")

csv_names <- names(wind_data)

for (lista in seq(length(wind_data))) {
  wind_data[[lista]] |>
  write.csv(paste0(csv_names[lista], ".csv"), row.names = FALSE)
}
