# Set environment ---------------------------------------------------------
rm(list = ls())
Sys.setenv("LANGUAGE" = "En")
Sys.setlocale("LC_ALL", "English")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep = "/")
FiguresDir    <- paste(BaseDir, "Figures", sep = "/")
ResultsDir    <- paste(BaseDir, "Results", sep = "/")
DataDir       <- paste(BaseDir, "Data",sep = "/")

# load Packages
setwd(CodesDir)
source("checkpackages.R")
source('decomp_stack_pred.R')
source("stack_pred.R")

packages <- c('vmd','dplyr','tidyverse','magrittr', 'caret', 'reshape2', 'gghighlight',
              'TTR', 'forecast', 'Metrics', 'e1071', "cowplot", "elmNNRcpp", "hht", "grid",
              "tcltk", "foreach", "iterators","doParallel","lmtest","magrittr", "ggpubr")

sapply(packages,packs)

rm(packages)

library(extrafont)
# font_import(pattern = 'CM')
library(ggplot2)
library(Cairo)

# Data treatment ----------------------------------------------------------
# set working directory
setwd(DataDir) 

# load data
raw_data <- list()

for (dataset in seq(3)) {
  raw_data[[dataset]] <- readxl::read_excel('dataset.xlsx', sheet = dataset)
}
names(raw_data) <- c('SSA', 'VMD', 'VMD_SSA')

# set to Results directory
setwd(ResultsDir)

# set a list of dates to analyze
months <- c(3,4,5)
dates <- paste0('2020-',months,'-01') %>% format.Date(., "%Y-%m-%d") %>% as.Date()

# create empty lists
wind_data <- list()
decomp_stack_results <- list()
stack_results <- list()

# list of models
model_list <- c(
  'ridge',
  'svmLinear2',
  'knn',
  'pls'
) %>% sort()
meta_model <- 'cubist'
horizon <- c(1,3,6)

count <- 1
for (dataset in seq(3)) {  
  for (month in seq(months)) {
    # filtering the data according to month list
    wind_data[[count]] <- raw_data[[dataset]][months(raw_data[[dataset]]$TimeStamp) %in% month.name[months[month]],] %>% as.data.frame()
    names(wind_data)[[count]] <- paste(names(raw_data)[dataset], month.name[months[month]], sep = '-')
    
    # training using ceemd stack
    decomp_stack_results[[count]] <- decomp_stack_pred(wind_data[[count]], model_list, meta_model, horizon)
    names(decomp_stack_results)[[count]] <- names(wind_data)[[count]]

    # save results
    saveRDS(
      object = decomp_stack_results[[count]],
      file = paste0('results_',names(raw_data)[dataset],'_',dates[month],'_stack.rds')
    )

    # training using stack models
    if (count %in% c(1:3)) {
      stack_results[[count]] <- stack_pred(wind_data[[count]], model_list, meta_model, horizon)
      names(stack_results)[[count]] <- month.name[months[month]]

      # save results
      saveRDS(
        object = stack_results[[count]],
        file = paste0('results_stack_',dates[month],'.rds')
      )
    }
    
    cat("\n\nModel:", names(raw_data)[dataset], '\tMonth:', month.name[months[month]])
    
    count <- count + 1
  }
}

# Save metrics ------------------------------------------------------------
# set working directory
setwd(ResultsDir)

# loop to save metrics results
FH <- paste0(horizon, '-steps') # aux to create forecasting horizon column
# FH <- c('One-step','Two-steps','Three-steps') # aux to create forecasting horizon column

count <- 1
for (dataset in seq(3)) {
  for (month in seq(months)) {
    filename_decomp_stack <- paste0('metrics_', names(raw_data)[dataset],'_',dates[month],'_stack.csv') # decomp stack file name  
    file.create(filename_decomp_stack) # create file decomp
    
    # append header in csv files
    data.frame('model','FH','MAE','MAPE','RMSE') %>%
      write.table(file = filename_decomp_stack,
                  append = TRUE,
                  sep = ',',
                  col.names = FALSE,
                  row.names = FALSE)
    
    for (metric in seq(decomp_stack_results[[count]]$Decomp_Metrics)) {
      # save decomp stack metrics in csv
      data.frame(
        model = paste0(names(raw_data)[dataset],'-STACK'),
        FH = rep(FH[metric]),
        decomp_stack_results[[count]]$STACK_Metrics[[metric]] %>% as.data.frame() %>% t()
      ) %>%
        write.table(file = filename_decomp_stack,
                    append = TRUE,
                    sep = ',',
                    row.names = FALSE,
                    col.names = FALSE)
      
      # save decomp metrics in csv
      data.frame(
        FH = rep(FH[metric]),
        decomp_stack_results[[count]]$Decomp_Metrics[[metric]][,-1]
      ) %>%
        write.table(file = filename_decomp_stack,
                    append = TRUE,
                    sep = ',',
                    row.names = TRUE,
                    col.names = FALSE)
      
    }
    count <- count + 1
  }
}

for (dataset in seq(months)) {
  filename_stack <- paste0('metrics_stack_',dates[dataset],'.csv') # stack file name
  file.create(filename_stack) # create file stack
  data.frame('model','FH','MAE','MAPE','RMSE') %>%
    write.table(file = filename_stack,
                append = TRUE,
                sep = ',',
                col.names = FALSE,
                row.names = FALSE)
  
  for (metric in seq(stack_results[[dataset]]$Metrics)) {
    # save stack metrics in csv
    data.frame(
      model = 'STACK',
      FH = rep(FH[metric]),
      stack_results[[dataset]]$STACK_Metrics[[metric]] %>% as.data.frame() %>% t()
    ) %>%
      write.table(file = filename_stack,
                  append = TRUE,
                  sep = ',',
                  col.names = FALSE,
                  row.names = FALSE)
    # save single metrics in csv
    data.frame(
      FH = rep(FH[metric]),
      stack_results[[dataset]]$Metrics[[metric]][,-1]
    ) %>%
      write.table(file = filename_stack,
                  append = TRUE,
                  sep = ',',
                  col.names = FALSE,
                  row.names = TRUE)
  }
}


# Load data --------------------------------------------------------------------
setwd(ResultsDir)

## load decomp data
file_list <- list.files(pattern = 'stack.rds') # list the .rds files
decomp_stack_results <- list()

for (dataset in seq(file_list)) {
  decomp_stack_results[[dataset]] <- readRDS(file = file_list[dataset])
}

## load stack data
file_list <- list.files(pattern = 'results_stack')
stack_results <- list()
for (dataset in seq(file_list)) {
  stack_results[[dataset]] <- readRDS(file = file_list[dataset])
}

## Plot Predict x Observed test ----
setwd(FiguresDir)
datasets24h <- list()

# datasets for 24h sets
for (ii in seq(3)) {
  datasets24h[[ii]] <- data.frame(
    'Observed' = tail(decomp_stack_results[[ii+6]]$Predictions$`1-step`[,'Obs'], 144),
    'OSA'      = tail(decomp_stack_results[[ii+6]]$Predictions$`1-step`[,2], 144),
    'Observed' = tail(decomp_stack_results[[ii+6]]$Predictions$`3-step`[,'Obs'], 144),
    'TSA'      = tail(decomp_stack_results[[ii+6]]$Predictions$`3-step`[,2], 144),
    'Observed' = tail(decomp_stack_results[[ii+6]]$Predictions$`6-step`[,'Obs'], 144),
    'SSA'      = tail(decomp_stack_results[[ii+6]]$Predictions$`6-step`[,2], 144)
  ) %>% melt() %>% data.frame(
    rep(seq(144)),
    .,
    rep(c('Observed','Predicted'), each = 144),
    rep(c("10 minutes","30 minutes","60 minutes"), each = 2*144)
  )
  
  datasets24h[[ii]]$variable <- NULL
  datasets24h[[ii]]$Set <- month.name[months[ii]]
  colnames(datasets24h[[ii]]) <- c('x','value', 'type', 'FH','Set')
}

names(datasets24h) <- month.name[months]

final_dataset <- do.call(rbind, datasets24h)

rownames(final_dataset) <- NULL

# plot test set 
final_dataset$FH <- final_dataset$FH %>% factor(levels = c("10 minutes","30 minutes","60 minutes"))
final_dataset$Set <- final_dataset$Set %>% factor(levels = month.name[months], labels = c(paste(month.name[months], "2020")))

plot_test <- final_dataset %>% as.data.frame %>% 
  ggplot(aes(x = x, y = value, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.background = element_blank(),
        legend.text = element_text(size = 20),
        text = element_text(family = "CM Roman", size = 20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
  ) +
  ylab('Wind Speed (m/s)') +
  xlab('Time sampling (10 minutes)') +
  scale_x_continuous(breaks = seq(0, 144, 48)) + 
  facet_grid(rows = vars(FH), cols = vars(Set), scales = "free") +
  scale_color_manual(values = c("#377EB8","#E41A1C")) +
  coord_cartesian(clip = "off") +
  guides(colour = guide_legend(override.aes = list(size=5)))

plot_test

plot_test %>% 
  ggsave(
    filename = paste0('PO.pdf'),
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  )

## Plot IMFs ----
setwd(FiguresDir)

IMFs <- data.frame()
Aux <- data.frame()

for (dataset in seq(3)) {
  Aux <- decomp_stack_results[[dataset+6]]$IMF
  Aux$dataset <- rep(dates[dataset])
  Aux$n <- seq(nrow(Aux))
  IMFs <- rbind(IMFs,Aux %>% melt(id.vars = c('dataset','n')))
}

dataset_labels <-
  c(
    expression(paste(March, " 2020")),
    expression(paste(April, " 2020")),
    expression(paste(May, " 2020"))
  )

IMFs$dataset <- IMFs$dataset %>% 
  factor(labels = dataset_labels)

# IMFs$dataset <- IMFs$dataset %>% 
#   factor(labels = c(paste(month.name[months], "2020")))

imf_labels <- 
  c(
    expression(paste(c[1])),
    expression(paste(c[2])),
    expression(paste(c[3])),
    expression(paste(c[4])),
    expression(paste(c[5]))
  )

IMFs$variable <- IMFs$variable %>% 
  factor(
    levels = c('Obs', paste0('c',seq(5))),
    labels = c('Obs',imf_labels)
  )

imf_plot <- IMFs %>% 
  filter(variable != 'Obs') %>%
  ggplot(aes(x = n, y = value, colour = variable)) +
  geom_line(size = 0.3, colour = '#377EB8') +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman", size = 16),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y = element_text(size = 18),
    panel.grid.minor = element_blank(),
  ) +
  ylab('') + xlab('Time sampling (10 minutes)') +
  facet_grid(
    variable ~ dataset,
    scales = 'free',
    switch = 'y',
    labeller = "label_parsed",
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(4))

# imf_plot

imf_plot %>% 
  ggsave(
    filename = 'imf_plot.pdf',
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  ) 

## Plot datasets ----
setwd(FiguresDir)

obs_dataset <- data.frame()
Aux2 <- data.frame()

for (dataset in seq(3)) {
  Aux2 <- data.frame(obs = wind_data[[dataset]][,'Original'])
  Aux2$n <- seq(nrow(wind_data[[dataset]]))
  Aux2$type <- c(rep('Training', times = nrow(wind_data[[dataset]])-1008), 
                        rep('Test', times = 1008))
  Aux2$dataset <- rep(paste0('dataset', dataset))
  obs_dataset <- rbind(obs_dataset, Aux2)
}

obs_dataset$dataset <- obs_dataset$dataset %>% 
  factor(
    levels = paste0('dataset', seq(3)),
    labels = c(paste0(month.name[months], " 2020"))
  )

dataplot <- obs_dataset %>% 
  ggplot(aes(x = n, y = obs)) +
  geom_line(size = 0.5, colour = '#377EB8') +
  facet_grid(vars(dataset)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.background = element_blank(),
        legend.text = element_text(size = 20),
        text = element_text(family = "CM Roman", size = 20),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 20),
  ) +
  ylab('Wind Speed (m/s)') +
  xlab('Time sampling (10 minutes)')

dataplot

dataplot %>% 
  ggsave(
    filename = 'datasets_plot.pdf',
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  ) 

## Plot errors' std radarplot ----
setwd(FiguresDir)

error <- list()
for (dataset in seq(ceemd_stack_results)) {
  error[[dataset]] <- data.frame()
  for (horizon in seq(ceemd_stack_results[[dataset]]$Errors)) {
    Aux4 <- tail(ceemd_stack_results[[dataset]]$Errors[[horizon]], 1008) %>% 
      apply(., 2, sd) %>% data.frame %>% t()
    Aux4 <- cbind(
      Aux4,
      tail(stack_results[[dataset]]$Errors[[horizon]], 1008) %>% 
        apply(., 2, sd) %>% data.frame %>% t()
    )
    
    error[[dataset]] <- rbind(error[[dataset]], Aux4)
  }
  rownames(error[[dataset]]) <- paste0(seq(3), '0 minutes')
  colnames(error[[dataset]]) <- LETTERS[1:14]
  
  error[[dataset]][,'F'] <- NULL 
  
  write.csv(
    x = error[[dataset]],
    file = paste0('dataset',dataset ,'_std.csv'),
    row.names = FALSE
  )
  
}
names(error) <- month.name[months]

## DM test ----
error <- list()
DM_tvalue <- list()
DM_pvalue <- list()
DM_presult <- list()
for (dataset in seq(ceemd_stack_results)) {
  error[[dataset]] <- list()
  DM_tvalue[[dataset]] <- list()
  DM_pvalue[[dataset]] <- list()
  DM_presult[[dataset]] <- list()
  for (horizon in seq(ceemd_stack_results[[dataset]]$Errors)) {
    # taking errors from ceemd_stack
    error[[dataset]][[horizon]] <- tail(ceemd_stack_results[[dataset]]$Errors[[horizon]], 1008)
    # cbind the errors from ceemd_stack with stack
    error[[dataset]][[horizon]] <- cbind(error[[dataset]][[horizon]], tail(stack_results[[dataset]]$Errors[[horizon]], 1008))
    # rename columns
    colnames(error[[dataset]][[horizon]]) <- LETTERS[1:ncol(error[[dataset]][[horizon]])]
    
    # DM test
    DM_tvalue[[dataset]][[horizon]] <- matrix(
      nrow = ncol(error[[dataset]][[horizon]]),
      ncol = ncol(error[[dataset]][[horizon]])
    )
    colnames(DM_tvalue[[dataset]][[horizon]]) <- LETTERS[1:ncol(error[[dataset]][[horizon]])]
    rownames(DM_tvalue[[dataset]][[horizon]]) <- LETTERS[1:ncol(error[[dataset]][[horizon]])]
    DM_pvalue[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]
    DM_presult[[dataset]][[horizon]] <- DM_tvalue[[dataset]][[horizon]]
    
    for (col in seq(nrow(DM_tvalue[[dataset]][[horizon]]))) {
      for (row in seq(nrow(DM_tvalue[[dataset]][[horizon]]))) {
        if (col == row) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else if (error[[dataset]][[horizon]][,col] == error[[dataset]][[horizon]][,row]) {
          DM_tvalue[[dataset]][[horizon]][row, col] <- NA
          DM_pvalue[[dataset]][[horizon]][row, col] <- NA
        } else {
          DMtest <- dm.test(
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
  names(error[[dataset]]) <- paste0(seq(3),'-step')
  names(DM_tvalue[[dataset]]) <- paste0(seq(3),'-step')
  names(DM_pvalue[[dataset]]) <- paste0(seq(3),'-step')
  names(DM_presult[[dataset]]) <- paste0(seq(3),'-step')
}
names(error) <- month.name[8:10]
names(DM_tvalue) <- month.name[8:10]
names(DM_pvalue) <- month.name[8:10]
names(DM_presult) <- month.name[8:10]
