

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
