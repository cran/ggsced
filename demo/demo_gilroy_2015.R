library(ggsced)
library(ggh4x)

data <- Gilroyetal2015

y_mult = .075
x_mult = .02

phase_labels <- ggsced_prep_labels(
  data = data,
  facet_col = "Participant",
  x_col = "Session",
  y_col = "Responding",
  default_hadj = 0.5,
  default_vadj = 0,
  specs = list(
    Andrew = list(
      list(label = "Andrew", x = 28, y = 0, hadj = 1, vadj = 0),
      list(label = "Baseline", x = 2.5, y = 100, vadj = 0),
      list(label = "Treatment", x = 9, y = 100, vadj = 0),
      list(label = "Maintenance", x = 18.5, y = 100, vadj = 0),
      list(label = "Generalization", x = 26, y = 100, vadj = 0)
    ),
    Brian = list(
      list(label = "Brian", x = 28, y = 0, hadj = 1, vadj = 0)
    ),
    Charles = list(
      list(label = "Charles", x = 28, y = 0, hadj = 1, vadj = 0)
    )
  )
)

p = ggplot(data, aes(Session, Responding,
                     group = Condition)) +

  geom_line() +
  geom_point(size = 3, pch = 16) +
  scale_x_continuous(breaks = c(1:28),
                     limits = c(1, 28),
                     guide = guide_axis(cap = "both"),
                     expand = expansion(mult = x_mult)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 100),
                     guide = guide_axis(cap = "both"),
                     expand = expansion(mult = y_mult)) +
  facet_grid(Participant ~ .,
             scales = "free_y",
             axis.labels = "margins",
             axes = "all")  +
  geom_text(data = phase_labels,
            mapping = aes(label = label,
                          hjust = hadj,
                          vjust = vadj,
                          fontface = fontface),
            show.legend = FALSE,
            family = "Times New Roman") +
  labs(x = "Session", y = "Percent Accuracy") +
  theme_classic() +
  theme(
    text = element_text(size = 14,
                        family = "Times New Roman",
                        color = "black"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

staggered_pls = list(
  '1' = c(4.5,   11.5,   18.5),
  '2' = c(13.5,  20.5,   23.5),
  '3' = c(23.5,  23.5,   23.5)
)

ggsced(p, legs = staggered_pls)
