mean_ip <-
  readr::read_csv(here::here("new_ip", "mean_ip.csv")) |>
  dplyr::mutate(models = as.factor(LETTERS[1:24]))

pop_ip_plot <-
  mean_ip |>
  ggplot2::ggplot(ggplot2::aes(x = mean_ip, y = reorder(models, mean_ip))) +
  ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = models)) +
  ggplot2::geom_point(size = 4, color = 'orange') +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(mean_ip, 4)*100,'%')),
    hjust = -.3,
    family = 'Times'
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times"),
    legend.position = 'None',
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 15),
    # axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
    panel.grid.major.y = ggplot2::element_blank(),
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0,0)
  ) +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::labs(x = 'Average Improvement Performance', y = 'Models') +
  ggplot2::coord_cartesian(xlim = c(0, 0.75))

pop_ip_plot |>
  ggplot2::ggsave(
    filename = here::here("new_ip", "avr_ip.pdf"),
    device = "pdf",
    width = 8,
    height = 4.5,
    units = "in",
    dpi = 1200
  )
