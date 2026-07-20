## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  dev = "png",
  warning = FALSE,
  message = FALSE
)

## ----setup, include=FALSE, echo=FALSE-----------------------------------------
library(ggsced)
library(tidyverse)
library(ggh4x)

## ----demo-gilroy-2015-step-1, eval=TRUE---------------------------------------
library(ggsced)
library(tidyverse)

data_set <- Gilroyetal2015

# Illustrate the structure of the dataset
head(data_set)

## ----demo-gilroy-2015-step-2, eval=TRUE---------------------------------------
# Create condition labels for the plot
data_labels <- data_set %>%
  select(Participant, Condition) %>%
  filter(Participant == "Andrew") %>%
  unique() %>%
  mutate(
    x = c(2.5, 9, 18.5, 25.5),
    y = 100
  )

participant_labels <- data_set %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(27, 3),
    y = 0
  )

# Create the base ggplot
p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(data = data_labels,
            mapping = aes(x, y, label = Condition)) +
  geom_text(data = participant_labels,
            mapping = aes(x, y,
              label = Participant
            ),
            inherit.aes = FALSE,
            hjust = 1,
            vjust = 0
          ) +
  facet_grid(rows = vars(Participant),
             axes = "all",
             axis.labels = "margins")

p

## ----demo-gilroy-2015-step-3, eval=TRUE---------------------------------------
# Create condition labels for the plot
data_labels <- data_set %>%
  select(Participant, Condition) %>%
  filter(Participant == "Andrew") %>%
  unique() %>%
  mutate(
    x = c(2.5, 9, 18.5, 25.5),
    y = 100
  )

participant_labels <- data_set %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(27, 3),
    y = 0
  )

# Create the base ggplot
p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(
    data = data_labels,
    mapping = aes(x, y, label = Condition)
  ) +
  geom_text(
    data = participant_labels,
    mapping = aes(x, y,
      label = Participant
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0
  ) +
  facet_grid(rows = vars(Participant),
             axes = "all",
             axis.labels = "margins")

# Define staggered phase change lines for multiple baseline design
staggered_pls <- list(
  "1" = c(4.5, 11.5, 18.5),
  "2" = c(13.5, 20.5, 23.5),
  "3" = c(23.5, 23.5, 23.5)
)

# Add phase change lines using ggsced
ggsced(p, staggered_pls)

## ----demo-gilroy-2015-step-4, eval=TRUE---------------------------------------
# Create condition labels for the plot
data_labels <- data_set %>%
  select(Participant, Condition) %>%
  filter(Participant == "Andrew") %>%
  unique() %>%
  mutate(
    x = c(2.5, 9, 18.5, 25.5),
    y = 97.5
  )

participant_labels <- data_set %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(27, 3),
    y = 0
  )

# Set plot scaling parameters
y_mult <- .05
x_mult <- .02

# Create the base ggplot
p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(
    data = data_labels,
    mapping = aes(x, y, label = Condition),
    hjust = 0.5,
    vjust = 0.0625
  ) +
  geom_text(
    data = participant_labels,
    mapping = aes(x, y,
      label = Participant
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0
  ) +
  scale_y_continuous(
    name = "Percentage Accuracy",
    limits = c(0, 100),
    breaks = (0:4) * 25,
    expand = expansion(mult = y_mult),
    guide = guide_axis(cap = "both")
  ) +
  scale_x_continuous(
    breaks = c(1:27),
    limits = c(1, 27),
    expand = expansion(mult = x_mult),
    guide = guide_axis(cap = "both")
  ) +
  facet_grid(rows = vars(Participant),
             axes = "all",
             axis.labels = "margins") +
  theme(
    text = element_text(size = 14, color = "black"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

# Define staggered phase change lines for multiple baseline design
staggered_pls <- list(
  "1" = c(4.5, 11.5, 18.5),
  "2" = c(13.5, 20.5, 23.5),
  "3" = c(23.5, 23.5, 23.5)
)

# Add phase change lines using ggsced
final_plot <- ggsced(p, staggered_pls)

## ----demo-gilroy-2021-step-1, eval=TRUE---------------------------------------
library(ggsced)
library(tidyverse)

data <- Gilroyetal2021

# Illustrate the structure of the dataset
head(data)

## ----demo-gilroy-2021-step-2, eval=TRUE---------------------------------------
data <- Gilroyetal2021

data_labels <- data %>%
  select(Participant, Condition) %>%
  filter(Participant == "John") %>%
  unique() %>%
  mutate(
    x = c(2, 5, 8, 11, 14, 18),
    Label = gsub("2", "", Condition),
    y = 20
  )

series_labels <- data %>%
  filter(Participant == "John") %>%
  select(Participant, Condition) %>%
  slice(1:2) %>%
  mutate(
    x0 = c(20.5, 20.5),
    x1 = c(19.5, 19.5),
    y = c(15, 5),
    Label = c("Responses", "Reinforcers")
  )

participant_labels <- data %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(25, 3),
    y = c(19.5, 9.5, 0)
  )

p <- ggplot(data, aes(Session, Responding,
  group = Condition
)) +
  geom_line() +
  geom_point(
    size = 2.5,
    pch = 21,
    fill = "black"
  ) +
  geom_line(
    mapping = aes(Session, Reinforcers),
    lty = 2
  ) +
  geom_point(
    mapping = aes(Session, Reinforcers),
    size = 2.5,
    pch = 24,
    fill = "white"
  ) +
  geom_text(
    data = data_labels,
    mapping = aes(x, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = series_labels,
    aes(x = x0, y, xend = x1, yend = y),
    arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text(
    data = series_labels,
    hjust = 0,
    mapping = aes(
      x = x0 + 0.1, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = participant_labels,
    mapping = aes(x, y,
      label = Participant
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0
  ) +
  facet_grid(rows = vars(Participant),
             scales = "free_y",
             axes = "all",
             axis.labels = "margins")

p

## ----demo-gilroy-2021-step-3, eval=TRUE---------------------------------------
data <- Gilroyetal2021

data_labels <- data %>%
  select(Participant, Condition) %>%
  filter(Participant == "John") %>%
  unique() %>%
  mutate(
    x = c(2, 5, 8, 11, 14, 18),
    Label = gsub("2", "", Condition),
    y = 20
  )

series_labels <- data %>%
  filter(Participant == "John") %>%
  select(Participant, Condition) %>%
  slice(1:2) %>%
  mutate(
    x0 = c(20.5, 20.5),
    x1 = c(19.5, 19.5),
    y = c(15, 5),
    Label = c("Responses", "Reinforcers")
  )

participant_labels <- data %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(25, 3),
    y = c(19.5, 9.5, 0)
  )

p <- ggplot(data, aes(Session, Responding,
  group = Condition
)) +
  geom_line() +
  geom_point(
    size = 2.5,
    pch = 21,
    fill = "black"
  ) +
  geom_line(
    mapping = aes(Session, Reinforcers),
    lty = 2
  ) +
  geom_point(
    mapping = aes(Session, Reinforcers),
    size = 2.5,
    pch = 24,
    fill = "white"
  ) +
  geom_text(
    data = data_labels,
    mapping = aes(x, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = series_labels,
    aes(x = x0, y, xend = x1, yend = y),
    arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text(
    data = series_labels,
    hjust = 0,
    mapping = aes(
      x = x0 + 0.1, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = participant_labels,
    mapping = aes(x, y,
      label = Participant
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0
  ) +
  scale_x_continuous(
    breaks = c(1:25),
    limits = c(1, 25),
    expand = expansion(mult = x_mult)
  ) +
  facet_grid(rows = vars(Participant),
             scales = "free_y",
             axes = "all",
             axis.labels = "margins")

staggered_pls <- list(
  "1" = c(3.5, 3.5, 3.5),
  "2" = c(6.5, 6.5, 8.5),
  "3" = c(9.5, 9.5, 11.5),
  "4" = c(12.5, 16.5, 16.5),
  "5" = c(15.5, 22.5, 19.5)
)

# Note: see the non-zero number in the last list element below
offsets_pls <- list(
  "1" = c(F, F, F),
  "2" = c(F, F, F),
  "3" = c(F, F, F),
  "4" = c(F, F, F),
  "5" = c(T, F, F)
)

ggsced(p, legs = staggered_pls, offs = offsets_pls)

## ----demo-gilroy-2021-step-4, eval=TRUE---------------------------------------
rm(list = ls())

library(ggsced)
library(tidyverse)

data <- Gilroyetal2021

y_mult <- .05
x_mult <- .02

data_labels <- data %>%
  select(Participant, Condition) %>%
  filter(Participant == "John") %>%
  unique() %>%
  mutate(
    x = c(2, 5, 8, 11, 14, 18),
    Label = gsub("2", "", Condition),
    y = 20
  )

series_labels <- data %>%
  filter(Participant == "John") %>%
  select(Participant, Condition) %>%
  slice(1:2) %>%
  mutate(
    x0 = c(20.5, 20.5),
    x1 = c(19.5, 19.5),
    y = c(15, 5),
    Label = c("Responses", "Reinforcers")
  )

participant_labels <- data %>%
  select(Participant) %>%
  unique() %>%
  mutate(
    x = rep(25, 3),
    y = c(19.5, 9.5, 0)
  )

p <- ggplot(data, aes(Session, Responding,
  group = Condition
)) +
  geom_line() +
  geom_point(
    size = 2.5,
    pch = 21,
    fill = "black"
  ) +
  geom_line(
    mapping = aes(Session, Reinforcers),
    lty = 2
  ) +
  geom_point(
    mapping = aes(Session, Reinforcers),
    size = 2.5,
    pch = 24,
    fill = "white"
  ) +
  geom_text(
    data = data_labels,
    mapping = aes(x, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = series_labels,
    aes(x = x0, y, xend = x1, yend = y),
    arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text(
    data = series_labels,
    hjust = 0,
    mapping = aes(
      x = x0 + 0.1, y,
      label = Label
    ),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = participant_labels,
    mapping = aes(x, y,
      label = Participant
    ),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0
  ) +
  scale_x_continuous(
    breaks = c(1:25),
    limits = c(1, 25),
    expand = expansion(mult = x_mult)
  ) +
  facet_grid(rows = vars(Participant),
             scales = "free_y",
             axes = "all",
             axis.labels = "margins") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(
        name = "Frequency",
        breaks = c(0, 10, 20),
        limits = c(0, 20),
        expand = expansion(mult = y_mult),
        guide = guide_axis(cap = "both")
      ),
      scale_y_continuous(
        name = "Frequency",
        breaks = c(0, 5, 10),
        limits = c(0, 10),
        expand = expansion(mult = y_mult),
        guide = guide_axis(cap = "both")
      ),
      scale_y_continuous(
        name = "Frequency",
        breaks = c(0, 10, 20),
        limits = c(0, 20),
        expand = expansion(mult = y_mult),
        guide = guide_axis(cap = "both")
      )
    )
  ) +
  theme(
    text = element_text(
      size = 14,
      color = "black"
    ),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

staggered_pls <- list(
  "1" = c(3.5, 3.5, 3.5),
  "2" = c(6.5, 6.5, 8.5),
  "3" = c(9.5, 9.5, 11.5),
  "4" = c(12.5, 16.5, 16.5),
  "5" = c(15.5, 22.5, 19.5)
)

offsets_pls <- list(
  "1" = c(F, F, F),
  "2" = c(F, F, F),
  "3" = c(F, F, F),
  "4" = c(F, F, F),
  "5" = c(T, F, F)
)

ggsced(p, legs = staggered_pls, offs = offsets_pls)

