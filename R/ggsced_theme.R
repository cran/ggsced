##
## Copyright 2026 Shawn Gilroy, Louisiana State University
##
## This file is part of ggsced.
##
## ggsced is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## ggsced is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with ggsced  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.

#' ggsced_style_y
#'
#' This is a convenience element designed to support a more complete interpretation of SCED-specific style conventions through ggplot. Specifically, an axis is drawn with a specific manner of expansion to support the broken axis (i.e., non-connected x/y) expected of SCED-specific publication guidelines.
#'
#' @param expansion Percentage of axis to pad (top/bottom) to break axis (Note: normalized parent units)
#' @param lwd Width of axis line
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Annotation to simulate a disconnected y-axis via expansion
#'
#' @importFrom ggplot2 annotation_custom
#' @export
#'
ggsced_style_y <- function(expansion = 0.00, lwd = 2, col = "black") {
  .Deprecated("ggplot2::scale_y_...")
  ggplot2::annotation_custom(ggsced_internal_y_axis(expansion,
                                                    lwd,
                                                    col))
}

#' ggsced_style_x
#'
#' This is a convenience element designed to support a more complete interpretation of SCED-specific style conventions through ggplot. Specifically, an axis is drawn with a specific manner of expansion to support the broken axis (i.e., non-connected x/y) expected of SCED-specific publication guidelines.
#'
#' @param expansion Percentage of axis to pad (left/right) to break axis (Note: normalized parent units)
#' @param lwd Width of axis line
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Annotation to simulate a disconnected y-axis via expansion
#'
#' @importFrom ggplot2 annotation_custom
#' @export
#'
ggsced_style_x <- function(expansion = 0.00, lwd = 2, col = "black") {
  .Deprecated("ggplot2::scale_x_...")
  ggplot2::annotation_custom(ggsced_internal_x_axis(expansion,
                                                    lwd,
                                                    col))
}
