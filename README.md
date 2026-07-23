# ggsced

[![License: MIT](https://img.shields.io/badge/License-GPL2-blue.svg)](https://opensource.org/licenses/gpl-2-0)
[![R package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![Codecov test coverage](https://codecov.io/gh/miyamot0/ggsced/graph/badge.svg)](https://app.codecov.io/gh/miyamot0/ggsced)

**Utilities and helpers for Single-Case Experimental Design (SCED) using ggplot2**

## Overview

The `ggsced` package extends the powerful `ggplot2` visualization framework to provide specialized tools for creating high-quality graphics for Single-Case Experimental Design (SCED) research. SCED studies are a crucial methodology in behavioral and educational research, where individual participants serve as their own controls through carefully designed experimental phases. This approaches rests on careful visual inspection of data presented in graphs that clearly delineate phase changes and patterns.

## Purpose

Single-case experimental designs require specific visualization conventions that are not easily achieved with standard plotting approaches. The `ggsced` package bridges the gap between the flexibility of `ggplot2` and the specific visualization needs of single-case researchers by providing:

- **Professional Phase Change Lines**: Clear visual demarcation between experimental phases that meet publication standards
- **Multiple Baseline Design Support**: Staggered intervention implementation across participants with precise phase change timing
- **Complex Data Pattern Visualization**: Support for multiple dependent variables plotted simultaneously
- **Publication-Ready Graphics**: APA and journal-compliant figures with consistent styling

## Key Features

### Core Visualization Functions

- **`ggsced()`**: Primary function for adding phase change lines to existing ggplot objects
- **`ggsced_style_x()`** and **`ggsced_style_y()`**: Styling functions for axes that follow SCED conventions with broken axis appearance

### Design Pattern Support

- **Multiple Baseline Designs**: Staggered intervention implementation across participants
- **Alternating Treatment Designs**: Rapid alternation between different intervention conditions
- **Functional Analysis Designs**: Multiple dependent variables with distinct visual markers
- **Complex Phase Patterns**: Support for multiple intervention phases within studies

### Professional Styling

- **Consistent Visual Standards**: Publication-quality aesthetics with proper fonts and scaling
- **Broken Axis Convention**: SCED-specific axis styling with disconnected x/y axes
- **Flexible Customization**: Modular design allows adaptation to diverse research needs
- **Multi-panel Support**: Seamless integration with `ggplot2` faceting for participant comparisons

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("miyamot0/ggsced")

# Load the package
library(ggsced)
```

## Quick Start

### Example Multiple Baseline: Gilroy et al. (2015)

```r
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
```

![Gilroy et al. (2015) Figure](https://github.com/miyamot0/ggsced/blob/main/figs/GilroyEtAl2015.png?raw=true)

### Example Augmented Multiple Baseline: Gilroy et al. (2021)

```r
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
```

![Gilroy et al. (2021) Figure](https://github.com/miyamot0/ggsced/blob/main/figs/GilroyEtAl2021.png?raw=true)

## Other Example Datasets

The package includes several real research datasets for learning and demonstration:

- **`Gilroyetal2015`**: Multiple baseline design data
- **`Gilroyetal2021`**: Cross-lagged Alternating Treatment Design

## Documentation

- **Comprehensive vignette**: Detailed examples with real research data
- **Function documentation**: Complete help files for all exported functions
- **Demo files**: Executable examples in the `demo/` directory
- **Test suite**: Extensive testing to ensure reliability

## (Minimal) Dependencies

- `ggplot2`: Core plotting functionality
- `grid`: Low-level graphics operations
- `gtable`: Plot layout management
- `dplyr`: Flexible preparation of labels and annotations
- `ggh4x`: Extended ggplot2 functionality (multiple y position scales; Optional)

## Author and Contact

**Shawn P. Gilroy, Ph.D.**  
Louisiana State University  
📧 [sgilroy1@lsu.edu](mailto:sgilroy1@lsu.edu)  
🆔 ORCID: [0000-0002-1097-8366](https://orcid.org/0000-0002-1097-8366)

## Bug Reports and Feature Requests

If you encounter any issues or have suggestions for improvements, please:

1. **Check existing issues**: Browse the [GitHub Issues](https://github.com/miyamot0/ggsced/issues) to see if your issue has already been reported
2. **Create a new issue**: If your issue is new, please [open a new issue](https://github.com/miyamot0/ggsced/issues/new) with:
   - A clear, descriptive title
   - Detailed description of the problem or feature request
   - Minimal reproducible example (if reporting a bug)
   - Your session info (`sessionInfo()`)
   - Expected vs. actual behavior

## License

This package is licensed under the GPL License (V2+).

## Citation

If you use `ggsced` in your research, please cite it appropriately:

```
Gilroy, S. P. (2026). ggsced: Utilities and helpers for Single-Case
Experimental Design using ggplot2. R package version 0.1.7.
https://github.com/miyamot0/ggsced
```

## Acknowledgments

This package was developed to support the single-case research community with publication-quality visualization tools. Special thanks to the researchers who provided data for demonstration examples and to the broader R community for the foundation provided by `ggplot2` and related packages.

---

**Keywords**: single-case design, SCED, behavioral research, data visualization, ggplot2, R package
