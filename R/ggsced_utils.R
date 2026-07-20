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

#' ggsced_extract_domain
#'
#' Internal helper to pull respective panel parameters and assert validity.
#'
#' @param panel_params respective parameters of panel grob
#'
#' @return numeric vector specifying size in min-max units
#' @importFrom assert assert
#'
ggsced_extract_domain <- function(panel_params) {
  assert::assert(is.list(panel_params),
                 msg = "gTree params must be of list type")

  x_range <- panel_params$x.range

  assert::assert(is.numeric(x_range),
                 length(x_range) == 2)

  return(x_range)
}

#' ggsced_scale_units
#'
#' Internal helper to discern exactly where a vertical would need to be placed within a specific grob (i.e., panel grob). Uses the domain and phase change location as a reference to convert to npc units for gtable and grid.
#'
#' @param session_value Session associated with phase-change line
#' @param domain_size Total size of domain per panel specification
#'
#' @return A proportional value given value in a panel domain
#'
ggsced_scale_units <- function(session_value, domain_size) {
  (session_value - domain_size[1]) / (domain_size[2] - domain_size[1])
}

#' ggsced_get_panels
#'
#' Honestly, a bit of a 'hackish' solution to extract grobs that are associated with data to be drawn. Probably fragile and specific to modern gg implementations, but probably the close we'll get. Subject to change.
#'
#' @param ggplot_grobs list of grobs prepared by ggplotGrob
#'
#' @return filtered list of grobs restricted to faceted levels
#'
ggsced_get_panels <- function(ggplot_grobs) {
  lcl_panels = ggplot_grobs$layout
  lcl_panels = lcl_panels[grepl("^panel", ggplot_grobs$layout$name), ]

  return(lcl_panels)
}

#' ggsced_name_dogleg
#'
#' Grobs need to have unique names lest they get accidentally overwritten. This is a convenience naming function for the 'main' phase break within a facet level.
#'
#' @param panel name of the parent grob
#' @param index index of the phase change in sequence
#' @param n_leg leg in the phase change
#'
#' @return a name corresponding with unique phase-change grob element
#'
ggsced_name_dogleg <- function(panel, index, n_leg) {
  paste0(panel$name, "-phase.change-", index, '-leg-', n_leg)
}

#' ggsced_name_dogleg_lateral
#'
#' Grobs need to have unique names lest they get accidentally overwritten. This is a convenience naming function for the 'lateral' part of a phase break within a facet level.
#'
#' @param panel name of the parent grob
#' @param index index of the phase change in sequence
#' @param n_leg leg in the phase change
#'
#' @return a name corresponding with unique phase-change grob element
#'
ggsced_name_dogleg_lateral <- function(panel, index, n_leg) {
  paste0(panel$name, "-phase.change-", index, "-leg.lateral-", n_leg)
}
