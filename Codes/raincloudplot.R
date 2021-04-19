library(magrittr)
library(ggplot2)
library(extrafont)

# load data

rawdata <-
  readxl::read_excel(
    here::here("Data", "dataset.xlsx"),
    sheet = 1
  ) %>% 
  dplyr::select(TimeStamp, Original) %>% 
  dplyr::mutate(
    month = paste(
      month.name[as.Date(TimeStamp) %>% format("%m") %>% as.numeric],
      "2020"
    )
  )

rawdata$month <- rawdata$month %>% 
  factor(levels = unique(rawdata$month))


# We will use a function by Ben Marwick
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

raincloudplot <- 
  rawdata %>% 
  ggplot(aes(x = reorder(month, desc(Original)), y = Original, fill = month)) +
  # The half violins
  # geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8, 
  #                  scale = "width") +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  # The points
  geom_point(aes(y = Original, color = month), 
             position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
  # The boxplots
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
  # \n adds a new line which creates some space between the axis and axis title
  labs(y = "Wind Speed (m/s)", x = NULL) +
  # Removing legends
  guides(fill = FALSE, color = FALSE) +
  # Setting the limits of the y axis
  scale_y_continuous(limits = c(0, 15)) +
  # Picking nicer colours
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman"),
    axis.text = element_text(size = 16), 
    axis.text.y = element_text(angle = 90, hjust = .5),
    axis.title = element_text(size = 18),
    axis.title.y = element_blank(),
    axis.line.x = element_line(color="black"), 
    axis.line.y = element_line(color="black"),
    # panel.border = element_blank(),
    # panel.grid.major.x = element_blank(),                                          
    # panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
    plot.title = element_text(size = 18, vjust = 1, hjust = 0),
    legend.text = element_text(size = 12),          
    legend.title = element_blank(),                              
    legend.position = c(0.95, 0.15), 
    legend.key = element_blank(),
    legend.background = element_rect(color = "black", 
                                     fill = "transparent", 
                                     size = 2, linetype = "blank")
  ) +
  coord_flip()

raincloudplot

raincloudplot %>% 
  ggsave(
    filename = here::here("Figures", "raincloudplot.pdf"),
    device = "pdf",
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  )
