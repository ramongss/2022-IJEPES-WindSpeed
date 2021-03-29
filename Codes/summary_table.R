library(magrittr)

# load data

rawdata <-
  readxl::read_excel(
    here::here("Data", "dataset.xlsx"),
    sheet = 1
  ) %>% 
  dplyr::select(TimeStamp, Original) %>% 
  dplyr::mutate(
    month = month.name[as.Date(TimeStamp) %>% format("%m") %>% as.numeric]
  )

rawdata$month <- rawdata$month %>% 
  factor(levels = unique(rawdata$month))


summary_tt <- rawdata %>% 
  dplyr::mutate(
    dataset = ifelse(
      (as.Date(TimeStamp) %>% format("%d")) > "23",
      "Test",
      "Training"
    ) %>% factor(levels = c("Training", "Test"))
  ) %>%
  dplyr::group_by(month, dataset) %>% 
  dplyr::summarise(
    min = min(Original),
    mean = mean(Original),
    median = median(Original),
    std = sd(Original),
    max = max(Original),
    .groups = "drop"
  )

summary_w <- 
  rawdata %>% 
    dplyr::mutate(
      dataset = "Whole"
    ) %>%
    dplyr::group_by(month, dataset) %>% 
    dplyr::summarise(
      min = min(Original),
      mean = mean(Original),
      median = median(Original),
      std = sd(Original),
      max = max(Original),
      .groups = "drop"
    )

summary_table <- rbind.data.frame(summary_tt, summary_w)

print(xtable::xtable(summary_table), include.rownames = FALSE)
