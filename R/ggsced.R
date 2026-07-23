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

#' ggsced
#'
#' Core exported function to facilitate the drawing of phase change lines atop a ggplot object. Primarily designed to be used *after* the plot is finalized, with the lines being the last element drawn at the highest z index (i.e., atop all elements).
#'
#' @param plt ggplot object as typically designed/printed in userspace
#' @param legs list of 'legs' to be drawn
#' @param offs mapping of lines that require minor offset
#' @param print output result directly to graphical device (default = TRUE)
#' @param verbose Optional ability to view process output (debugging primarily)
#'
#' @return Finalized figure with respective phase change lines embedded.
#' @importFrom assert assert
#' @importFrom ggh4x facetted_pos_scales
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob
#' @import grid
#' @export
#'
ggsced <- function(plt, legs,
                   offs = NULL,
                   print = TRUE,
                   verbose = FALSE) {

  assert::assert(!is.null(plt),
                 ("ggplot" %in% class(plt)),
                 msg = "Plot object must be a valid ggplot object.")

  ggsced_output_console("\u2705 Passes check: ggplot object valid", verbose)

  assert::assert(!is.null(legs),
                 is.list(legs),
                 msg = "Phase change points must be a valid ordered list.")

  ggsced_output_console("\u2705 Passes check: Phase change list of correct type", verbose)

  assert::assert(is.logical(print))
  assert::assert(is.logical(verbose))

  # Actual GG object to reference
  lcl_ggplot_build <- ggplot2::ggplot_build(plt)
  assert::assert(!is.null(lcl_ggplot_build),
                 ("ggplot_built" %in% class(lcl_ggplot_build)),
                 msg = "Failed to execute ggplot_build. Malformed input?")

  ggsced_output_console("\u2705 GG Pre-render: Built successfully", verbose)

  # Grobs to be drawn on grid
  lcl_ggplot_grobs <- ggplot2::ggplotGrob(plt)
  assert::assert(!is.null(lcl_ggplot_grobs),
                 ("gTree" %in% class(lcl_ggplot_grobs)),
                 msg = "Failed to generate gTree from ggplot object. Malformed input?")

  ggsced_output_console("\u2705 GTree created: Built successfully", verbose)

  # Grobs specific to data to be annotated
  lcl_panels <- ggsced_get_panels(lcl_ggplot_grobs)

  # Number of panels as per the drawn figure
  lcl_n_panels <- nrow(lcl_panels)
  assert::assert(!is.null(lcl_n_panels),
                 lcl_n_panels > 0,
                 msg = "Failed to identify multiple panels")

  ggsced_output_console("\u2705 Panels Extracted: Multiple panels pulled from gTree", verbose)

  # Assert: Must be uniform length legs
  leg_lengths <- unlist(lapply(legs, function(vec) {
    assert::assert(all(is.numeric(vec)),
                   msg = "Phase change points must be of numeric type.")
    length(vec)
  }), use.names = FALSE)

  assert::assert(length(unique(leg_lengths)) == 1,
                 msg = "Phase change vectors in list are not of a uniform length.")

  ggsced_output_console("\u2705 Phase Change Legs: list of vectors of uniform length", verbose)

  if (!is.null(offs)) {
    # Assert: Must be uniform length legs
    offset_lengths <- unlist(lapply(offs, function(vec) {
      assert::assert(all(is.logical(vec)),
                     msg = "Phase change points must be of numeric type.")
      length(vec)
    }), use.names = FALSE)

    assert::assert(length(unique(offset_lengths)) == 1,
                   msg = "Phase change offset vectors in list are not of a uniform length.")

    assert::assert(unique(leg_lengths) == unique(offset_lengths),
                   length(leg_lengths) == length(offset_lengths),
                   msg = "Phase change and offset vectors may be identical in dimension")

    ggsced_output_console("\u2705 Phase Change Offsets: list of vectors of uniform length", verbose)
  }

  n_leg <- 0

  for (pl in legs) {
    n_leg <- n_leg + 1

    for (row in seq_len(lcl_n_panels)) {
      # Reference respective panel pulled from gTree
      lcl_panel <- lcl_panels[row,]

      # Confirm if there are panels that follow this one
      has_more_rows <- row < lcl_n_panels

      # Extract range from grob (presuming uniform)
      x_range <- ggsced_extract_domain(lcl_ggplot_build$layout$panel_params[[row]])

      # Scale x units to npc to derive desired location
      npc_x <- ggsced_scale_units(pl[row], x_range)

      # Dynamic b = dynamic 'bottom' in the gTable object (i.e., t:b, common l/r)
      # If MORE ROWS, should extend JUST ABOVE next panel (i.e., n_2 - t)
      # If LAST ROW, the b should just t for current panel (i.e., n = n)
      dynamic_b <- ifelse(has_more_rows == TRUE,
                          lcl_panels[row + 1, "t"] - 1,
                          lcl_panel$t)

      # Dynamic offs = offsets to typical 'lateral' phase lines
      # Note: This is uncommon, so more of a special case
      # Generally, should be TRUE to notch up VERY SLIGHTLY
      dynamic_offs <- ifelse(is.null(offs) == FALSE,
                             offs[[n_leg]][row],
                             FALSE)

      # TODO: Make this more clear/sane
      draw_short <- dynamic_offs

      if (draw_short == TRUE) {
        dynamic_b <- dynamic_b - 2

        # Note: This is the full segment
        main_segment_name <- ggsced_name_dogleg(lcl_panel, row, n_leg)
        main_segment <- sced_phase_change_main_panel_grob(npc_x, main_segment_name)
        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    b = dynamic_b,
                                                    z = 1000,
                                                    name = main_segment_name)

        # Note: This is linking to full segment if in shortened space
        main_segment_pre <- sced_phase_change_complex_lateral_pre_grob(npc_x,
                                                                       paste(main_segment_name,
                                                                             'pre-lateral'))
        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment_pre,
                                                    t = dynamic_b + 1,
                                                    l = lcl_panel$l,
                                                    z = 1000,
                                                    name = paste(main_segment_name, 'pre'))

        if (has_more_rows == TRUE) {
          main_segment_lateral_name <- ggsced_name_dogleg_lateral(lcl_panel, row, n_leg)
          npc_x2 <- ggsced_scale_units(pl[row + 1], x_range)

          # Note: This is linking to full segment if in shortened space
          main_segment_post <- sced_phase_change_complex_lateral_post_grob(npc_x2,
                                                                           paste(main_segment_lateral_name,
                                                                                 'post-lateral'))
          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      main_segment_post,
                                                      t = dynamic_b + 1,
                                                      l = lcl_panel$l,
                                                      z = 1000,
                                                      name = paste(main_segment_lateral_name,
                                                                   'post-lateral'))

          lateral_segment2 <- sced_phase_change_complex_lateral_grob(npc_x, npc_x2,
                                                                     paste0(main_segment_lateral_name,
                                                                            'lateral'))
          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment2,
                                                      t = lcl_panels[row + 1,]$t - 2,
                                                      l = lcl_panels[row + 1,]$l,
                                                      z = 1000,
                                                      name = paste0(main_segment_lateral_name,
                                                                    'lateral'))
        }
      } else {
        # Note: This is the full segment
        main_segment_name <- ggsced_name_dogleg(lcl_panel, row, n_leg)
        main_segment <- sced_phase_change_main_panel_grob(npc_x, main_segment_name)

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    b = dynamic_b,
                                                    z = 1000,
                                                    name = main_segment_name)

        if (has_more_rows == TRUE) {
          main_segment_lateral_name <- ggsced_name_dogleg_lateral(lcl_panel, row, n_leg)
          npc_x2 <- ggsced_scale_units(pl[row + 1], x_range)
          lateral_segment <- sced_phase_change_simple_lateral_grob(npc_x, npc_x2,
                                                                   main_segment_lateral_name)

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment,
                                                      t = lcl_panels[row + 1,]$t,
                                                      l = lcl_panels[row + 1,]$l,
                                                      z = 1000,
                                                      name = main_segment_lateral_name)
        }
      }
    }

    ggsced_output_console(paste0("\u2705 Drew Phase Change ",
                                 n_leg, " of ", lcl_n_panels),
                          verbose)
  }

  if (print == TRUE) {
    grid::grid.newpage()
    grid::grid.draw(lcl_ggplot_grobs)

    ggsced_output_console("\u2705 Figure Output: Drawn to graphical device", verbose)
  }

  ggsced_output_console("\u2705 Figure Output: Successfully output", verbose)

  invisible(lcl_ggplot_grobs)
}
