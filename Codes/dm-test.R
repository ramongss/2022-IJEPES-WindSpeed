library(magrittr)

# Create error dataframes ----

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
error[["March"]] <- list()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  error[["March"]][[horizon]] <- 
    cbind(
      tail(decomp_stack_results$`VMD_SSA-March`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`VMD-March`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`SSA-March`$Errors[[horizon]], 1008),
      tail(stack_results$March$Errors[[horizon]], 1008)
    ) %>% data.frame()
}

# April
error[["April"]] <- list()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  error[["April"]][[horizon]] <- 
    cbind(
      tail(decomp_stack_results$`VMD_SSA-April`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`VMD-April`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`SSA-April`$Errors[[horizon]], 1008),
      tail(stack_results$April$Errors[[horizon]], 1008)
    ) %>% data.frame()
}

# May
error[["May"]] <- list()
for (horizon in seq(decomp_stack_results[[dataset]]$Errors)) {
  error[["May"]][[horizon]] <- 
    cbind(
      tail(decomp_stack_results$`VMD_SSA-May`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`VMD-May`$Errors[[horizon]], 1008),
      tail(decomp_stack_results$`SSA-May`$Errors[[horizon]], 1008),
      tail(stack_results$May$Errors[[horizon]], 1008)
    ) %>% data.frame()
}

for (dataset in seq(3)) {
  for (horizon in seq(3)) {
    colnames(error[[dataset]][[horizon]]) <- LETTERS[1:20]
    rownames(error[[dataset]][[horizon]]) <- NULL
  }
  names(error[[dataset]]) <- paste0(c(1,3,6),"-step")
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
      nrow = ncol(error[[dataset]][[horizon]]),
      ncol = 1
    )
    colnames(DM_tvalue[[dataset]][[horizon]]) <- paste0(horizons[horizon], "0 min")
    rownames(DM_tvalue[[dataset]][[horizon]]) <- LETTERS[1:ncol(error[[dataset]][[horizon]])]
    DM_pvalue[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]
    DM_presult[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]
    
    for (col in seq(ncol(DM_tvalue[[dataset]][[horizon]]))) {
      for (row in seq(nrow(DM_tvalue[[dataset]][[horizon]]))) {
        if (col == row) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else if (error[[dataset]][[horizon]][,col] == error[[dataset]][[horizon]][,row]) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else {
          DMtest <- forecast::dm.test(
            error[[dataset]][[horizon]][,col],
            error[[dataset]][[horizon]][,row],
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
  names(DM_tvalue[[dataset]]) <- names(error[[dataset]])
  names(DM_pvalue[[dataset]]) <- names(error[[dataset]])
  names(DM_presult[[dataset]]) <- names(error[[dataset]])
}

names(DM_tvalue) <- names(error)
names(DM_pvalue) <- names(error)
names(DM_presult) <- names(error)

tvalue <- do.call(cbind, do.call(cbind, DM_tvalue))
pvalue <- do.call(cbind, do.call(cbind, DM_pvalue))
presult <- do.call(cbind, do.call(cbind, DM_presult))

tvalue %>% 
  write.csv(here::here("Results", "DM_tvalue.csv"),
            row.names = T,
            col.names = T)

pvalue %>% 
  write.csv(here::here("Results", "DM_pvalue.csv"),
            row.names = T,
            col.names = T)

presult %>% 
  write.csv(here::here("Results", "DM_presult.csv"),
            row.names = T,
            col.names = T)

xtable::xtable(tvalue)
