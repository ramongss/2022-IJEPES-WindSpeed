library(magrittr)

## load decomp data
file_list <- 
  paste0(here::here("Results"), "/",
         list.files(path = here::here("Results"), pattern = 'stack.rds')) # list the .rds files
decomp_stack_results <- list()
for (dataset in seq(file_list)) {
  decomp_stack_results[[dataset]] <- readRDS(file = file_list[dataset])
}

## load stack data
file_list <- 
  paste0(here::here("Results"), "/",
         list.files(path = here::here("Results/"), pattern = 'results_stack'))
stack_results <- list()
for (dataset in seq(file_list)) {
  stack_results[[dataset]] <- readRDS(file = file_list[dataset])
}

datasets <- c('SSA', 'VMD', 'VMD_SSA')
months <- month.name[c(3,4,5)]

## rename lists
count <- 1
for (dataset in seq(datasets)) {
  for (month in seq(months)) {
    names(decomp_stack_results)[[count]] <- 
      paste(datasets[dataset], months[month], sep = '-')
    
    if (count %in% c(1:3)) {
      names(stack_results)[[count]] <- months[month]
    }
    
    count <- count + 1
  }
}

error <- list()

# March
error[["March"]] <- data.frame()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  Aux <- 
  cbind(
    tail(decomp_stack_results$`VMD_SSA-March`$Errors[[horizon]], 1008) %>%
      apply(., 2, sd) %>% data.frame %>% t(),
    tail(decomp_stack_results$`VMD-March`$Errors[[horizon]], 1008) %>%
      apply(., 2, sd) %>% data.frame %>% t(),
    tail(decomp_stack_results$`SSA-March`$Errors[[horizon]], 1008) %>%
      apply(., 2, sd) %>% data.frame %>% t(),
    tail(stack_results$March$Errors[[horizon]], 1008) %>%
      apply(., 2, sd) %>% data.frame %>% t()
  )
  error[["March"]] <- rbind(error[["March"]], Aux)
}

# April
error[["April"]] <- data.frame()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  Aux <- 
    cbind(
      tail(decomp_stack_results$`VMD_SSA-April`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(decomp_stack_results$`VMD-April`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(decomp_stack_results$`SSA-April`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(stack_results$April$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t()
    )
  error[["April"]] <- rbind(error[["April"]], Aux)
}

# May
error[["May"]] <- data.frame()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  Aux <- 
    cbind(
      tail(decomp_stack_results$`VMD_SSA-May`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(decomp_stack_results$`VMD-May`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(decomp_stack_results$`SSA-May`$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t(),
      tail(stack_results$May$Errors[[horizon]], 1008) %>%
        apply(., 2, sd) %>% data.frame %>% t()
    )
  error[["May"]] <- rbind(error[["May"]], Aux)
}

for (dataset in seq(3)) {
  colnames(error[[dataset]]) <- LETTERS[1:20]
  rownames(error[[dataset]]) <- NULL
  
  error[[dataset]] <-
  error[[dataset]] %>%
    dplyr::mutate(Month = rep(months[dataset]),
                  FH = paste0(c(1,3,6), '0 minutes'),
                  .before = 1)
  
  # write.csv(
  #   x = error[[dataset]],
  #   file = here::here("Codes", paste0('dataset',dataset ,'_std.csv')),
  #   row.names = FALSE
  # )
}

errors <- do.call(rbind, error)
rownames(errors) <- NULL