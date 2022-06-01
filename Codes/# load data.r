# load data
raw_data <- list()

for (dataset in seq(3)) {
  raw_data[[dataset]] <-
    readxl::read_excel(here::here('Data', 'dataset.xlsx'), sheet = dataset) |>
    dplyr::mutate(month = lubridate::floor_date(TimeStamp, "month") |> as.Date()) |>
    `colnames<-`(c('TimeStamp', 'c1', 'c2', 'c3', 'c4', 'c5', 'Residual', 'Reconstr', 'Original', 'month')) |>
    dplyr::select(month, TimeStamp, c1, c2, c3, c4, c5, Original)
}
names(raw_data) <- c('SSA', 'VMD', 'VMD_SSA')

# set a list of dates to analyze
months <- unique(raw_data$SSA$month)

wind_data <- list()

counter <- 1

for (dataset in seq(raw_data)) {
  for (month_ in seq(length(months))) {
    wind_data[[counter]] <-
      raw_data[[dataset]] |>
      dplyr::filter(month == months[month_])

    names(wind_data)[[counter]] <-
      paste(
        names(raw_data)[dataset],
        format(months[month_], "%B"),
        sep = '-'
      )

    counter <- counter + 1
  }
}

wind_data |> saveRDS("Data/data.rds")
