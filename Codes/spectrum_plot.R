library(magrittr)
library(ggplot2)
library(extrafont)

# load ssa function
source(here::here("Codes", "ssascontr.R"))

Y <- 
  readxl::read_excel(
    here::here("Data", "dataset.xlsx"),
    sheet = 1
  ) %>% 
  dplyr::select(Original) %>% 
  dplyr::pull(Original)

ssa <- 
  data.frame(x = seq(12),
             y = round(ssascontr(Y, 12), 5)) %>% 
  dplyr::mutate(label = y*100)

plot <- ssa %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 1, colour = '#377EB8') +
  geom_point(size = 3, colour = '#E41A1C') +
  ggrepel::geom_text_repel(
    aes(label = paste0("lambda[",x,"]","*\'=\'~",label,"*\'%\'")),
    parse = T,
    force = T
  ) +
  theme_bw() +
  theme(
    text = element_text(family = 'CM Roman', size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_log10(
    labels = scales::trans_format('log10', scales::math_format(10^.x)),
  ) +
  scale_x_continuous(
    breaks = seq(1, nrow(ssa), 1),
    limits = c(1, nrow(ssa))
    # expand = c(0, 0)
  ) +
  annotation_logticks(sides = 'l') +
  xlab('Components number') + ylab('Normalized spectrum')

plot %>% ggsave(
  filename = here::here("Figures", "spectrum.pdf"),
  device = 'pdf',
  width = 12,
  height = 6.75
)
