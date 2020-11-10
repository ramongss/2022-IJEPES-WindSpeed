ssa <- data.frame(x = seq(50), y = ssascontr(Y, 50))
plot <- ssa %>% ggplot(aes(x = x, y = y)) +
  geom_line(size = 1, colour = '#377EB8') +
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
    breaks = seq(0, nrow(ssa), 5),
    limits = c(0, nrow(ssa))
    # expand = c(0, 0)
  ) +
  annotation_logticks(sides = 'l') +
  xlab('Components number') + ylab('Normalized spectrum')

plot %>% ggsave(
  filename = 'spectrum.png',
  device = 'png',
  width = 12,
  height = 6.75
)
