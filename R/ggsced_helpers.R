

#' ggsced_facet_labels
#'
#' Optional helper class to prepare facet labels (e.g., Participant names)
#' based on the faceting of an existing ggplot object
#'
#' @param plt ggplot object
#' @param y ordinate of respective label (typically very low or very high)
#' @param x position on the x-axis (Default = last tick from plot object)
#'
#' @return a data frame that can be used with a geom_text layer
#' @importFrom ggplot2 ggplot_build
#' @export
#'
ggsced_facet_labels <- function(plt, y = 0, x = NULL) {
  .Deprecated("Deprecated in favor of ggsced_prep_labels")

  lcl_bld <- ggplot2::ggplot_build(plt)
  lcl_plot_data <- lcl_bld$plot$data

  max_x <- max(lcl_bld$layout$panel_params[[1]]$x$breaks)
  min_y <- y

  if (!is.null(x)) max_x <- x

  facet_name <- names(lcl_bld$layout$facet_params$rows)[1]
  unique_names <- unique(lcl_plot_data[, facet_name])

  x_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["x"]]))
  y_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["y"]]))
  g_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["group"]]))

  tag_frm <- list()
  tag_frm[[facet_name]] <- unique_names
  tag_frm[["label"]] <- as.character(unique_names)
  tag_frm[[x_col]] <- rep(max_x, length(unique_names))
  tag_frm[[y_col]] <- rep(min_y, length(unique_names))
  tag_frm[[g_col]] <- rep(1, length(unique_names))

  tag_frm_df <- as.data.frame(tag_frm)

  tag_frm_df
}

#' generate_condition_labels
#'
#' Optional helper class to prepare condition labels (e.g., Baseline,
#' Intervention) based on the grouping of an existing ggplot object
#'
#' @param plt ggplot object
#' @param y ordinate of respective label (Default = last tick from plot object)
#'
#' @return a data frame that can be used with a geom_text layer
#' @importFrom ggplot2 ggplot_build
#' @export
#'
ggsced_condition_labels <- function(plt, y = NULL) {
  .Deprecated("Deprecated in favor of ggsced_prep_labels")

  lcl_bld <- ggplot2::ggplot_build(plt)
  lcl_plot_data <- lcl_bld$plot$data

  max_y <- max(lcl_bld$layout$panel_params[[1]]$y$breaks)

  if (!is.null(y)) max_y <- y

  facet_name <- names(lcl_bld$layout$facet_params$rows)[1]
  unique_names <- unique(lcl_plot_data[, facet_name])

  x_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["x"]]))
  y_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["y"]]))
  g_col <- gsub("~", "", deparse(lcl_bld$plot$mapping[["group"]]))

  levels_of_facet <- levels(lcl_bld$plot$data[, facet_name])

  levels_of_grp <- levels(lcl_bld$plot$data[, g_col])

  lcl_data_internal <- lcl_bld$data[[1]]
  lcl_data_internal <- lcl_data_internal[lcl_data_internal$PANEL == 1, ]

  facet_name <- names(lcl_bld$layout$facet_params$rows)[1]
  unique_names <- unique(lcl_plot_data[, facet_name])

  splits_by_g <- by(lcl_data_internal,
    list(lcl_data_internal$group),
    function(df_g) {
      lcl_x_min <- min(df_g$x)
      lcl_x_max <- max(df_g$x)

      current_lvl <- as.numeric(unique(df_g$group))
      x_set <- mean(c(lcl_x_min, lcl_x_max))

      list(x = x_set,
           label = levels_of_grp[current_lvl],
           group = 1,
           y = max_y)
    }
  )

  df_splits <- do.call(rbind, splits_by_g)
  df_splits_df <- as.data.frame(df_splits)

  tag_frm <- list()
  tag_frm[[x_col]] <- as.numeric(df_splits_df$x)
  tag_frm[[y_col]] <- as.numeric(df_splits_df$y)
  tag_frm[[g_col]] <- rep(1, nrow(df_splits_df))
  tag_frm[[facet_name]] <- factor(rep(levels_of_facet[1], nrow(df_splits_df)),
                                  levels = levels_of_facet)
  tag_frm[["label"]] <- as.character(df_splits_df$label)

  tag_frm_df <- as.data.frame(tag_frm)

  tag_frm_df
}


#' ggsced_prep_labels
#'
#' This function prepares a data frame for plotting labels in a ggplot object
#' based on specified facets and label specifications. It validates the input
#' parameters, checks for the presence of required columns, and constructs a new
#' data frame containing the necessary information for plotting labels according
#' to the provided specifications.
#'
#' @param data A data frame containing the data to be used for plotting.
#' @param facet_col The name of the column in `data` that contains the facet values.
#' @param specs A named list where each name corresponds to a facet value and each entry is a list of label specifications. Each label specification should include at least the `label`, `x`, and `y` values, and can optionally include other aesthetics like `hadj`, `vadj`, `text_size`, and `fontface`.
#' @param x_col The name of the column in `data` that contains the x-axis values (default is "Session").
#' @param y_col The name of the column in `data` that contains the y-axis values (default is "Value").
#' @param default_hadj The default horizontal adjustment to use for labels if not specified in the `specs` (default is 0.5).
#' @param default_vadj The default vertical adjustment to use for labels if not specified in the `specs` (default is 0.5).
#' @param default_text_size The default text size to use for labels if not specified in the `specs` (default is NA_real_).
#' @param default_font_face The default font face to use for labels if not specified in the `specs` (default is "plain").
#'
#' @returns A data frame containing the prepared label specifications for plotting, with additional columns for any extra aesthetics specified in the `specs`.
#'
#' @import assert
#' @import dplyr
#'
#' @export
ggsced_prep_labels <- function(data,
                               facet_col,
                               specs,
                               x_col = "Session",
                               y_col = "Value",
                               default_hadj = 0.5,
                               default_vadj = 0.5,
                               default_text_size = NA_real_,
                               default_font_face = "plain") {
  assert::assert(
    is.data.frame(data),
    msg = "`data` must be a data.frame."
  )

  assert::assert(
    is.character(facet_col),
    length(facet_col) == 1,
    facet_col %in% names(data),
    msg = "`facet_col` must be a single column name present in `data`."
  )

  assert::assert(
    is.character(x_col),
    length(x_col) == 1,
    x_col %in% names(data),
    msg = "`x_col` must be a single column name present in `data`."
  )

  assert::assert(
    is.character(y_col),
    length(y_col) == 1,
    y_col %in% names(data),
    msg = "`y_col` must be a single column name present in `data`."
  )

  assert::assert(
    is.numeric(default_hadj),
    length(default_hadj) == 1,
    !is.na(default_hadj),
    msg = "`default_hadj` must be a single numeric value."
  )

  assert::assert(
    is.numeric(default_vadj),
    length(default_vadj) == 1,
    !is.na(default_vadj),
    msg = "`default_vadj` must be a single numeric value."
  )

  assert::assert(
    is.numeric(default_text_size),
    length(default_text_size) == 1,
    is.na(default_text_size) || default_text_size > 0,
    msg = "`default_text_size` must be a single positive numeric value or NA."
  )

  assert::assert(
    is.character(default_font_face),
    length(default_font_face) == 1,
    !is.na(default_font_face),
    msg = "`default_font_face` must be a single character value."
  )

  assert::assert(
    is.list(specs),
    length(specs) > 0,
    !is.null(names(specs)),
    msg = "`specs` must be a non-empty named list."
  )

  spec_names <- names(specs)
  assert::assert(
    !any(is.na(spec_names)),
    !any(spec_names == ""),
    msg = "Each entry in `specs` must have a non-empty name matching a facet value."
  )

  facet_values <- unique(as.character(data[[facet_col]]))
  unknown_facets <- setdiff(spec_names, facet_values)
  assert::assert(
    length(unknown_facets) == 0,
    msg = paste0(
      "Unknown facet key(s) in `specs`: ",
      paste(unknown_facets, collapse = ", "),
      "."
    )
  )

  output_rows <- vector("list", length = 0)

  for (facet_key in spec_names) {
    facet_idx <- which(as.character(data[[facet_col]]) == facet_key)
    if (length(facet_idx) == 0) {
      next
    }

    template_row <- data[facet_idx[1], , drop = FALSE]
    spec_block <- specs[[facet_key]]

    is_single_entry <- is.list(spec_block) &&
      !is.null(names(spec_block)) &&
      all(c("label", "x", "y") %in% names(spec_block))

    if (is_single_entry) {
      entries <- list(spec_block)
    } else if (is.list(spec_block)) {
      entries <- spec_block
    } else {
      assert::assert(
        FALSE,
        msg = "Each facet entry in `specs` must be a named list or list of named lists."
      )
    }

    for (entry in entries) {
      assert::assert(
        is.list(entry),
        !is.null(names(entry)),
        msg = "Each label specification must be a named list."
      )

      assert::assert(
        all(c("label", "x", "y") %in% names(entry)),
        msg = "Each label specification must include `label`, `x`, and `y`."
      )

      if (!is.null(entry[["font_face"]]) && is.null(entry[["fontface"]])) {
        entry[["fontface"]] <- entry[["font_face"]]
      }

      if (is.null(entry[["hadj"]])) {
        entry[["hadj"]] <- default_hadj
      }

      if (is.null(entry[["vadj"]])) {
        entry[["vadj"]] <- default_vadj
      }

      if (is.null(entry[["text_size"]])) {
        entry[["text_size"]] <- default_text_size
      }

      if (is.null(entry[["fontface"]])) {
        entry[["fontface"]] <- default_font_face
      }

      row_out <- template_row
      row_out[[facet_col]] <- if (is.factor(data[[facet_col]])) {
        factor(facet_key, levels = levels(data[[facet_col]]))
      } else {
        facet_key
      }

      row_out[[x_col]] <- entry[["x"]]
      row_out[[y_col]] <- entry[["y"]]
      row_out[["label"]] <- as.character(entry[["label"]])

      extra_fields <- setdiff(names(entry), c("label", "x", "y"))
      for (field_name in extra_fields) {
        row_out[[field_name]] <- entry[[field_name]]
      }

      output_rows[[length(output_rows) + 1]] <- row_out
    }
  }

  assert::assert(
    length(output_rows) > 0,
    msg = "No label rows were produced. Check `specs` and `facet_col` values."
  )

  output_df <- dplyr::bind_rows(output_rows)

  base_cols <- names(data)
  extra_cols <- setdiff(names(output_df), base_cols)
  output_df <- output_df[, c(base_cols, extra_cols), drop = FALSE]

  output_df
}


#' ggsced_prep_lines
#'
#' This function prepares a data frame for plotting lines in a ggplot object
#' based on specified facets and line specifications. It validates the input
#' parameters, checks for the presence of required columns, and constructs a new
#' data frame containing the necessary information for plotting lines according
#' to the provided specifications.
#'
#' @param data A data frame containing the data to be used for plotting.
#' @param facet_col The name of the column in `data` that contains the facet values.
#' @param specs A named list where each name corresponds to a facet value and each entry is a list of line specifications. Each line specification should include at least the `x` value and can optionally include other aesthetics like `linetype`.
#' @param x_col The name of the column in `data` that contains the x-axis values (default is "Session").
#' @param default_linetype The default linetype to use for lines if not specified in the `specs` (default is 1).
#'
#' @returns A data frame containing the prepared line specifications for plotting, with additional columns for any extra aesthetics specified in the `specs`.
#'
#' @import assert
#'
#' @export
ggsced_prep_lines <- function(data,
                              facet_col,
                              specs,
                              x_col = "Session",
                              default_linetype = 1) {
  assert::assert(
    is.data.frame(data),
    msg = "`data` must be a data.frame."
  )

  assert::assert(
    is.character(facet_col),
    length(facet_col) == 1,
    facet_col %in% names(data),
    msg = "`facet_col` must be a single column name present in `data`."
  )

  assert::assert(
    is.character(x_col),
    length(x_col) == 1,
    x_col %in% names(data),
    msg = "`x_col` must be a single column name present in `data`."
  )

  assert::assert(
    (is.numeric(default_linetype) || is.character(default_linetype)),
    length(default_linetype) == 1,
    !is.na(default_linetype),
    msg = "`default_linetype` must be a single numeric or character value."
  )

  assert::assert(
    is.list(specs),
    length(specs) > 0,
    !is.null(names(specs)),
    msg = "`specs` must be a non-empty named list."
  )

  spec_names <- names(specs)
  assert::assert(
    !any(is.na(spec_names)),
    !any(spec_names == ""),
    msg = "Each entry in `specs` must have a non-empty name matching a facet value."
  )

  facet_values <- unique(as.character(data[[facet_col]]))
  unknown_facets <- setdiff(spec_names, facet_values)
  assert::assert(
    length(unknown_facets) == 0,
    msg = paste0(
      "Unknown facet key(s) in `specs`: ",
      paste(unknown_facets, collapse = ", "),
      "."
    )
  )

  output_rows <- vector("list", length = 0)

  for (facet_key in spec_names) {
    facet_idx <- which(as.character(data[[facet_col]]) == facet_key)
    if (length(facet_idx) == 0) {
      next
    }

    template_row <- data[facet_idx[1], , drop = FALSE]
    spec_block <- specs[[facet_key]]

    is_single_entry <- is.list(spec_block) &&
      !is.null(names(spec_block)) &&
      ("x" %in% names(spec_block))

    if (is_single_entry) {
      entries <- list(spec_block)
    } else if (is.list(spec_block)) {
      entries <- spec_block
    } else {
      assert::assert(
        FALSE,
        msg = "Each facet entry in `specs` must be a named list or list of named lists."
      )
    }

    for (entry in entries) {
      assert::assert(
        is.list(entry),
        !is.null(names(entry)),
        msg = "Each line specification must be a named list."
      )

      assert::assert(
        "x" %in% names(entry),
        msg = "Each line specification must include `x`."
      )

      if (!is.null(entry[["line_type"]]) && is.null(entry[["linetype"]])) {
        entry[["linetype"]] <- entry[["line_type"]]
      }

      if (!is.null(entry[["lty"]]) && is.null(entry[["linetype"]])) {
        entry[["linetype"]] <- entry[["lty"]]
      }

      if (is.null(entry[["linetype"]])) {
        entry[["linetype"]] <- default_linetype
      }

      row_out <- template_row
      row_out[[facet_col]] <- if (is.factor(data[[facet_col]])) {
        factor(facet_key, levels = levels(data[[facet_col]]))
      } else {
        facet_key
      }

      row_out[[x_col]] <- entry[["x"]]

      extra_fields <- setdiff(names(entry), c("x"))
      for (field_name in extra_fields) {
        row_out[[field_name]] <- entry[[field_name]]
      }

      output_rows[[length(output_rows) + 1]] <- row_out
    }
  }

  assert::assert(
    length(output_rows) > 0,
    msg = "No line rows were produced. Check `specs` and `facet_col` values."
  )

  output_df <- dplyr::bind_rows(output_rows)

  base_cols <- names(data)
  extra_cols <- setdiff(names(output_df), base_cols)
  output_df <- output_df[, c(base_cols, extra_cols), drop = FALSE]

  output_df
}
