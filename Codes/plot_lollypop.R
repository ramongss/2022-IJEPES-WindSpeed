library(magrittr)
library(ggplot2)
library(extrafont)

metrics <- read.csv(here::here("Results", "metrics.csv"))

metrics <- metrics %>% 
  tidyr::unite(
    "dataset",
    dataset:criteria
  )

models_names <- colnames(metrics[,-1]) %>%
  toupper() %>% 
  gsub("_", "-", .)

# calculate improvement performance
ip <- data.frame()

for (row in seq(nrow(metrics[, -1]))) {
  for (col in seq(ncol(metrics[, -1]))) {
    ip[row, col] <- 
      (metrics[, -1][row, col] - min(metrics[, -1][row, ]))/
      metrics[, -1][row, col]
  }
}

ip <- cbind(metrics[, 1], ip)

colnames(ip) <- c("dataset", LETTERS[seq(length(models_names))])

mean_ip <-
  ip[, -1] %>% 
  apply(2, mean) %>% 
  data.frame(
    models = colnames(ip[, -1]),
    mean_ip = .
  ) %>%
  dplyr::arrange(desc(-mean_ip))

rownames(mean_ip) <- NULL

mean_ip$models <- mean_ip$models %>% 
  factor(levels = mean_ip$models)

# ip[, -1] %>%
#   data.frame(
#     max = apply(., 1, max),
#     min = apply(., 1, function(x) min(x[x != min(x)]))
#   )

# max_min <- data.frame(
#   max = apply(ip[, -1], 1, max),
#   min = apply(ip[, -1], 1, function(x) min(x[x != min(x)]))
# )
 
# data.frame(
#   max_max = max(max_min$max),
#   min_min = min(max_min$min)
# )

pop_ip_plot <- 
  mean_ip %>% 
  ggplot(aes(x = mean_ip, y = models)) +
  geom_segment(aes(xend = 0, yend = models)) +
  geom_point(size = 4, color = 'orange') +
  geom_text(
    aes(label = paste0(round(mean_ip, 4)*100,'%')), 
    hjust = -.3, 
    family = 'CM Roman'
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "CM Roman"),
    legend.position = 'None',
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    # axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.y = element_blank(),
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0,0)
  ) +
  scale_y_discrete(limits = rev) +
  labs(x = 'Average Improvement Performance', y = 'Models') +
  coord_cartesian(xlim = c(0, 0.65))

pop_ip_plot %>% 
  ggsave(
    filename = here::here("Figures", "avr_ip.pdf"),
    device = "pdf",
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  )
