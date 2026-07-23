
#' sced_phase_change_main_panel_grob
#'
#' This is a helper class to standardize drawing grobs
#'
#' @param x value corresponding to respective domain in npc units
#' @param segment_name name associated with object
#'
#' @return graphical object
#'
#' @import grid
#'
sced_phase_change_main_panel_grob <- function(x, segment_name) {
  grid::segmentsGrob(x0 = grid::unit(x, "npc"), x1 = grid::unit(x, "npc"),
                     y0 = grid::unit(1, "npc"), y1 = grid::unit(0, "npc"),
                     name = segment_name)
}

#' sced_phase_change_simple_lateral_grob
#'
#' This is a helper class to standardize drawing grobs
#'
#' @param x0 value corresponding to respective domain in npc units
#' @param x1 value corresponding to respective domain in npc units
#' @param segment_name name associated with object
#'
#' @return graphical object
#'
#' @import grid
#'
sced_phase_change_simple_lateral_grob <- function(x0, x1, segment_name) {
  grid::segmentsGrob(x0 = grid::unit(x0, "npc"),
                     x1 = grid::unit(x1, "npc"),
                     y0 = grid::unit(1, "npc"),
                     y1 = grid::unit(1, "npc"),
                     name = segment_name)
}

#' sced_phase_change_complex_lateral_grob
#'
#' This is a helper class to standardize drawing grobs
#'
#' @param x0 value corresponding to respective domain in npc units
#' @param x1 value corresponding to respective domain in npc units
#' @param segment_name name associated with object
#'
#' @return graphical object
#'
#' @import grid
#'
sced_phase_change_complex_lateral_grob <- function(x0, x1, segment_name) {
  grid::segmentsGrob(x0 = grid::unit(x0, "npc"),
                     x1 = grid::unit(x1, "npc"),
                     y0 = grid::unit(0.5, "npc"),
                     y1 = grid::unit(0.5, "npc"),
                     name = segment_name)
}

#' sced_phase_change_complex_lateral_pre_grob
#'
#' This is a helper class to standardize drawing grobs
#'
#' @param x value corresponding to respective domain in npc units
#' @param segment_name name associated with object
#'
#' @return graphical object
#'
#' @import grid
#'
sced_phase_change_complex_lateral_pre_grob <- function(x, segment_name) {
  grid::segmentsGrob(x0 = grid::unit(x, "npc"),
                     x1 = grid::unit(x, "npc"),
                     y0 = grid::unit(1, "npc"),
                     y1 = grid::unit(0.5, "npc"),
                     name = segment_name)
}

#' sced_phase_change_complex_lateral_post_grob
#'
#' This is a helper class to standardize drawing grobs
#'
#' @param x value corresponding to respective domain in npc units
#' @param segment_name name associated with object
#'
#' @return graphical object
#'
#' @import grid
#'
sced_phase_change_complex_lateral_post_grob <- function(x, segment_name) {
  grid::segmentsGrob(x0 = grid::unit(x, "npc"),
                     x1 = grid::unit(x, "npc"),
                     y0 = grid::unit(0.5, "npc"),
                     y1 = grid::unit(0, "npc"),
                     name = paste(segment_name, "post"))
}

#' ggsced_internal_x_axis
#'
#' Internal helper class to draw a conventional
#' x-axis per SCED publication suggestions.
#'
#' @param expansion Percentage of axis to pad (left/right)
#' to break axis (Note: normalized parent units)
#' @param lwd Width of axis line
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Rendering of customized x axis across facets
#'
#' @import grid
#'
ggsced_internal_x_axis <- function(expansion = 0.00, lwd = 2, col = "black") {
  .Deprecated("ggplot::scale_x_...")
  grid::linesGrob(x = grid::unit(c(expansion, 1 - expansion), "npc"),
                  y = c(0, 0),
                  gp = grid::gpar(lwd = lwd, col = col))
}

#' ggsced_internal_y_axis
#'
#' Internal helper class to draw a conventional
#' y-axis per SCED publication suggestions.
#'
#' @param expansion Percentage of axis to pad (top/bottom)
#' to break axis (Note: normalized parent units)
#' @param lwd Width of axis line
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Rendering of customized y axis across facets
#'
#' @import grid
#'
ggsced_internal_y_axis <- function(expansion = 0.00, lwd = 2, col = "black") {
  .Deprecated("ggplot::scale_y_...")
  grid::linesGrob(x = c(0, 0),
                  y = grid::unit(c(expansion, 1 - expansion), "npc"),
                  gp = grid::gpar(lwd = lwd, col = col))
}
