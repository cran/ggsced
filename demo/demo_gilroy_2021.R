library(ggsced)
library(ggh4x)

data <- Gilroyetal2021

y_mult = .05
x_mult = .02

phase_labels <- ggsced_prep_labels(
  data = data,
  facet_col = "Participant",
  x_col = "Session",
  y_col = "Responding",
  default_hadj = 0.5,
  default_vadj = 0,
  specs = list(
    John = list(
      list(label = "John", x = 25, y = 20, hadj = 1, vadj = 1),
      list(label = "Baseline", x = 2, y = 20),
      list(label = "FR-Lowest", x = 5, y = 20),
      list(label = "Baseline", x = 8, y = 20),
      list(label = "FR-Inelastic", x = 11, y = 20),
      list(label = "FR-Elastic", x = 14, y = 20),
      list(label = "FR-Inelastic", x = 17, y = 20),

      list(label = "Responding", x = 21, y = 15, hadj = 0, vadj = 0.5),
      list(label = "Reinforcers", x = 21, y = 5, hadj = 0, vadj = 0.5)
    ),
    Anthony = list(
      list(label = "Anthony", x = 25, y = 10, hadj = 1, vadj = 1)
    ),
    Charles = list(
      list(label = "Charles", x = 25, y = 10, hadj = 1, vadj = 1)
    )
  )
)

p = ggplot(data, aes(Session, Responding,
                     group = Condition)) +

  geom_line() +
  geom_point(size = 2.5,
             pch = 21,
             fill = 'black') +

  geom_line(mapping = aes(Session, Reinforcers),
            lty = 2) +
  geom_point(mapping = aes(Session, Reinforcers),
             size = 2.5,
             pch = 24,
             fill = 'white') +
  scale_x_continuous(breaks = c(1:25),
                     limits = c(1, 25),
                     guide = guide_axis(cap = "both"),
                     expand = expansion(mult = x_mult)) +
  facet_grid(Participant ~ .,
             scales = "free_y",
             axis.labels = "margins",
             axes = "all")  +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         guide = guide_axis(cap = "both"),
                         expand = expansion(mult = y_mult))
    )
  ) +
  geom_text(data = phase_labels,
            mapping = aes(label = label,
                          hjust = hadj,
                          vjust = vadj,
                          fontface = fontface),
            show.legend = FALSE,
            family = "Times New Roman") +
  theme_classic() +
  theme(
    text = element_text(size = 14,
                        family = "Times New Roman",
                        color = "black"),
    #panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

# Create extra rows for Bx Labels
# TODO: Line helper?
extra_labels_df <- phase_labels[1:2,]
extra_labels_df$Session <- 21.25
extra_labels_df$x0 <- 21
extra_labels_df$x1 <- 19.5
extra_labels_df$y <- 15

extra_labels_df[1, "label"] <- 'Responding'
extra_labels_df[1, "Responding"] <- 15

extra_labels_df[2, "label"] <- 'Reinforcers'
extra_labels_df[2, "Responding"] <- 5
extra_labels_df[2, "y"] <- 5

p <- p + geom_segment(data = extra_labels_df,
                      mapping = aes(x = x0,
                                    y,
                                    xend = x1,
                                    yend = y),
                      arrow = arrow(length = unit(0.25, "cm")))

staggered_pls = list(
  '1' = c(3.5,   3.5,   3.5),
  '2' = c(6.5,   6.5,   8.5),
  '3' = c(9.5,   9.5,  11.5),
  '4' = c(12.5,  16.5,  16.5),
  '5' = c(15.5,  22.5,  19.5)
)

offsets_pls = list(
  '1' = c(F, F, F),
  '2' = c(F, F, F),
  '3' = c(F, F, F),
  '4' = c(F, F, F),
  '5' = c(T, F, F)
)

ggsced(p, legs = staggered_pls, offs = offsets_pls)
