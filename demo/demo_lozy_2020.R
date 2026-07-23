library(ggsced)
library(tidyverse)
library(ggh4x)

pre_data_to_plot <- LozyEtAl2020Data

y_mult <- .075
x_mult <- .05

panel_labels <- ggsced_prep_labels(
  data = pre_data_to_plot,
  facet_col = "Participant",
  x_col = "Session",
  y_col = "Value",
  default_hadj = 1,
  default_vadj = 0,
  default_text_size = 5,
  default_font_face = "bold",
  specs = list(
    Al = list(label = "Al", x = 15, y = 0),
    Ari = list(label = "Ari", x = 30, y = 0),
    Cali = list(label = "Cali", x = 15, y = 0),
    Eli = list(label = "Eli", x = 15, y = 0),
    Eva = list(label = "Eva", x = 15, y = 0),
    Ry = list(label = "Ry", x = 15, y = 0)
  )
)

phase_labels <- ggsced_prep_labels(
  data = pre_data_to_plot,
  facet_col = "Participant",
  x_col = "Session",
  y_col = "Value",
  default_hadj = 0.5,
  default_vadj = 0,
  specs = list(
    Al = list(
      list(label = "Choice 1", x = 2.5, y = 10),
      list(label = "Choice 2", x = 6.5, y = 10)
    ),
    Ari = list(
      list(label = "Choice 1", x = 4.0, y = 15),
      list(label = "Choice 2", x = 16, y = 15)
    ),
    Cali = list(
      list(label = "Choice 1", x = 2.5, y = 10),
      list(label = "Choice 2", x = 10.5, y = 10)
    ),
    Eli = list(
      list(label = "Choice 1", x = 2.5, y = 15)
    ),
    Eva = list(
      list(label = "Choice 1", x = 2.5, y = 5),
      list(label = "Choice 2", x = 6.5, y = 5),
      list(label = "Choice 3", x = 10.5, y = 5)
    ),
    Ry = list(
      list(label = "Choice 1", x = 2.5, y = 10),
      list(label = "Choice 2", x = 11.5, y = 10)
    )
  )
)

panel_lines <- ggsced_prep_lines(
  data = pre_data_to_plot,
  facet_col = "Participant",
  x_col = "Session",
  default_linetype = 2,
  specs = list(
    Al = list(list(x = 4.5)),
    Ari = list(list(x = 11.5)),
    Cali = list(list(x = 8.5)),
    Eva = list(
      list(x = 4.5),
      list(x = 8.5)
    ),
    Ry = list(list(x = 9.5))
  )
)

ggplot(pre_data_to_plot, aes(Session, Value,
                             shape = Response,
                             fill = Response,
                             group = interaction(Response, Phase))) +
  geom_line() +
  geom_point(size = 2) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("white", "black")) +
  facet_wrap(~ Participant, ncol = 2,
             scales = "free",
             axes = "all",
             axis.labels = "margins") +
  facetted_pos_scales(
    x = list(
      scale_x_continuous(breaks = c(1, 5, 10, 15),
                         limits = c(1, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0))),
      scale_x_continuous(breaks = c(1, 10, 20, 30),
                         limits = c(1, 30),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0))),
      scale_x_continuous(breaks = c(1, 5, 10, 15),
                         limits = c(1, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0))),
      scale_x_continuous(breaks = c(1, 5, 10, 15),
                         limits = c(1, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0))),
      scale_x_continuous(breaks = c(1, 5, 10, 15),
                         limits = c(1, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0))),
      scale_x_continuous(breaks = c(1, 5, 10, 15),
                         limits = c(1, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(x_mult, 0)))
    ),
    y = list(
      scale_y_continuous(breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0))),
      scale_y_continuous(breaks = c(0, 5, 10, 15),
                         limits = c(0, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0))),
      scale_y_continuous(breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0))),
      scale_y_continuous(breaks = c(0, 5, 10, 15),
                         limits = c(0, 15),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0))),
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5),
                         limits = c(0, 5),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0))),
      scale_y_continuous(breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = c(y_mult, 0)))
    )
  ) +
  geom_text(data = panel_labels,
            mapping = aes(label = label,
                          hjust = hadj,
                          vjust = vadj,
                          fontface = fontface),
            size = panel_labels$text_size,
            show.legend = FALSE,
            family = "Times New Roman") +
  geom_text(data = phase_labels,
            mapping = aes(label = label,
                          hjust = hadj,
                          vjust = vadj,
                          fontface = fontface),
            show.legend = FALSE,
            family = "Times New Roman") +
  geom_vline(data = panel_lines,
             mapping = aes(xintercept = Session),
             linetype = panel_lines$linetype) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  labs(x = "Session",
       y = "Selections") +
  theme(
    legend.position = c(1, 0.5),
    legend.justification = c(1, 0.5),
    legend.title = element_blank(),
    legend.key.spacing.y = unit(-0.125, "cm"),
    text = element_text(size = 14,
                        family = "Times New Roman",
                        color = "black"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 5, l = 5),
    panel.spacing = unit(1, "lines")
  )
