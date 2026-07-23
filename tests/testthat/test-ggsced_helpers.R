test_that("Correct facet labels are generated", {
  data_set <- Gilroyetal2015

  y_mult <- .05
  x_mult <- .02

  p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
    geom_line() +
    geom_point(size = 3) +
    scale_y_continuous(name = "Percentage Accuracy",
                       limits = c(0, 100),
                       breaks = (0:4) * 25,
                       expand = expansion(mult = c(y_mult))) +
    scale_x_continuous(breaks = c(1:27),
                       limits = c(1, 27),
                       expand = expansion(mult = c(x_mult))) +
    facet_grid(rows = vars(Participant),
               axes = "all",
               axis.labels = "margins") +
    theme(text = element_text(size = 14, color = 'black'),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    ggsced_style_x(x_mult, lwd = 2) +
    ggsced_style_y(y_mult, lwd = 2)

  simple_facet_labels_df = ggsced_facet_labels(p)

  true_result_df = data.frame(
    Participant = factor(c("Andrew", "Brian", "Charles"),
                         levels = c("Andrew", "Brian", "Charles")),
    label = c("Andrew", "Brian", "Charles"),
    Session = c(27, 27, 27),
    Responding = c(0, 0, 0),
    Condition = c(1, 1, 1)
  )

  expect_identical(simple_facet_labels_df, true_result_df)
})

test_that("Correct facet labels are generated", {
  data_set <- Gilroyetal2015

  y_mult <- .05
  x_mult <- .02

  p <- ggplot(data_set, aes(Session, Responding, group = Condition)) +
    geom_line() +
    geom_point(size = 3) +
    scale_y_continuous(name = "Percentage Accuracy",
                       limits = c(0, 100),
                       breaks = (0:4) * 25,
                       expand = expansion(mult = c(y_mult))) +
    scale_x_continuous(breaks = c(1:27),
                       limits = c(1, 27),
                       expand = expansion(mult = c(x_mult))) +
    facet_grid(rows = vars(Participant),
               axes = "all",
               axis.labels = "margins") +
    theme(text = element_text(size = 14, color = 'black'),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    ggsced_style_x(x_mult, lwd = 2) +
    ggsced_style_y(y_mult, lwd = 2)

  simple_condition_labels_df <- ggsced_condition_labels(p)

  true_result_df <- data.frame(
    Session = c(2.5, 24, 18.5, 7.5),
    Responding = c(100, 100, 100, 100),
    Condition = c(1, 1, 1, 1),
    Participant = factor(c("Andrew", "Andrew", "Andrew", "Andrew"),
                         levels = c("Andrew", "Brian", "Charles")),
    label = c("Baseline", "Generalization", "Maintenance", "Treatment")
  )

  expect_identical(simple_condition_labels_df, true_result_df)
})

make_helper_fixture <- function(as_factor = TRUE) {
  participants <- c("P1", "P1", "P2", "P2")
  participant_col <- if (as_factor) {
    factor(participants, levels = c("P1", "P2", "P3"))
  } else {
    participants
  }

  data.frame(
    Participant = participant_col,
    Session = c(1, 2, 1, 2),
    Value = c(10, 12, 20, 22),
    Condition = c("Baseline", "Tx", "Baseline", "Tx"),
    stringsAsFactors = FALSE
  )
}

test_that("ggsced_prep_labels handles single entries and injects defaults", {
  data_set <- make_helper_fixture(as_factor = TRUE)

  specs <- list(
    P1 = list(label = "Panel 1", x = 2, y = 0),
    P2 = list(label = "Panel 2", x = 2, y = 0)
  )

  out <- ggsced_prep_labels(
    data = data_set,
    facet_col = "Participant",
    specs = specs,
    x_col = "Session",
    y_col = "Value"
  )

  expect_identical(names(out), c(
    "Participant", "Session", "Value", "Condition",
    "label", "hadj", "vadj", "text_size", "fontface"
  ))
  expect_equal(out$label, c("Panel 1", "Panel 2"))
  expect_equal(out$Session, c(2, 2))
  expect_equal(out$Value, c(0, 0))
  expect_equal(out$hadj, c(0.5, 0.5))
  expect_equal(out$vadj, c(0.5, 0.5))
  expect_true(all(is.na(out$text_size)))
  expect_equal(out$fontface, c("plain", "plain"))
  expect_s3_class(out$Participant, "factor")
  expect_identical(levels(out$Participant), c("P1", "P2", "P3"))
})

test_that("ggsced_prep_labels handles multiple entries, aliases, and custom defaults", {
  data_set <- make_helper_fixture(as_factor = FALSE)

  specs <- list(
    P1 = list(
      list(label = "A", x = 1.5, y = 5, font_face = "bold"),
      list(label = "B", x = 2.0, y = 6, hadj = 0.1, vadj = 0.2, text_size = 4)
    ),
    P2 = list(label = "C", x = 1.8, y = 7)
  )

  out <- ggsced_prep_labels(
    data = data_set,
    facet_col = "Participant",
    specs = specs,
    default_hadj = 0.25,
    default_vadj = 0.75,
    default_text_size = 3,
    default_font_face = "italic"
  )

  expect_equal(nrow(out), 3)
  expect_false(is.factor(out$Participant))
  expect_equal(out$label, c("A", "B", "C"))
  expect_equal(out$fontface, c("bold", "italic", "italic"))
  expect_equal(out$hadj, c(0.25, 0.1, 0.25))
  expect_equal(out$vadj, c(0.75, 0.2, 0.75))
  expect_equal(out$text_size, c(3, 4, 3))
})

test_that("ggsced_prep_labels validates inputs", {
  data_set <- make_helper_fixture(as_factor = TRUE)

  expect_error(
    ggsced_prep_labels(data = 1, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1))),
    "`data` must be a data.frame.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Missing", specs = list(P1 = list(label = "L", x = 1, y = 1))),
    "`facet_col` must be a single column name present in `data`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), x_col = "Missing"),
    "`x_col` must be a single column name present in `data`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), y_col = "Missing"),
    "`y_col` must be a single column name present in `data`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), default_hadj = NA_real_),
    "`default_hadj` must be a single numeric value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), default_vadj = "x"),
    "`default_vadj` must be a single numeric value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), default_text_size = 0),
    "`default_text_size` must be a single positive numeric value or NA.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(label = "L", x = 1, y = 1)), default_font_face = NA_character_),
    "`default_font_face` must be a single character value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = "bad"),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list()),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  bad_named_specs <- list(list(label = "L", x = 1, y = 1))
  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = bad_named_specs),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  bad_empty_name_specs <- list(P1 = list(label = "L", x = 1, y = 1))
  names(bad_empty_name_specs)[1] <- ""
  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = bad_empty_name_specs),
    "Each entry in `specs` must have a non-empty name matching a facet value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P4 = list(label = "L", x = 1, y = 1))),
    "Unknown facet key(s) in `specs`: P4.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(1, 2, 3))),
    "Each label specification must be a named list.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list(list(x = 1, y = 1)))),
    "Each label specification must include `label`, `x`, and `y`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_labels(data = data_set, facet_col = "Participant", specs = list(P1 = list())),
    "No label rows were produced. Check `specs` and `facet_col` values.",
    fixed = TRUE
  )
})

test_that("ggsced_prep_lines handles single entries and default linetype", {
  data_set <- make_helper_fixture(as_factor = TRUE)

  specs <- list(
    P1 = list(x = 1.5),
    P2 = list(x = 1.7)
  )

  out <- ggsced_prep_lines(
    data = data_set,
    facet_col = "Participant",
    specs = specs
  )

  expect_identical(names(out), c(
    "Participant", "Session", "Value", "Condition", "linetype"
  ))
  expect_equal(out$Session, c(1.5, 1.7))
  expect_equal(out$linetype, c(1, 1))
  expect_s3_class(out$Participant, "factor")
  expect_identical(levels(out$Participant), c("P1", "P2", "P3"))
})

test_that("ggsced_prep_lines handles aliases, precedence, and custom x column", {
  data_set <- make_helper_fixture(as_factor = FALSE)

  specs <- list(
    P1 = list(
      list(x = 1.5, line_type = 2),
      list(x = 1.8, lty = 3),
      list(x = 2.0, lty = 3, line_type = 2, linetype = 5)
    ),
    P2 = list(x = 2.2)
  )

  out <- ggsced_prep_lines(
    data = data_set,
    facet_col = "Participant",
    specs = specs,
    default_linetype = 7,
    x_col = "Session"
  )

  expect_equal(nrow(out), 4)
  expect_equal(out$linetype, c(2, 3, 5, 7))
  expect_equal(out$Session, c(1.5, 1.8, 2.0, 2.2))
  expect_false(is.factor(out$Participant))
})

test_that("ggsced_prep_lines validates inputs", {
  data_set <- make_helper_fixture(as_factor = TRUE)

  expect_error(
    ggsced_prep_lines(data = 1, facet_col = "Participant", specs = list(P1 = list(x = 1))),
    "`data` must be a data.frame.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Missing", specs = list(P1 = list(x = 1))),
    "`facet_col` must be a single column name present in `data`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P1 = list(x = 1)), x_col = "Missing"),
    "`x_col` must be a single column name present in `data`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P1 = list(x = 1)), default_linetype = NA),
    "`default_linetype` must be a single numeric or character value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = "bad"),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list()),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  bad_named_specs <- list(list(x = 1))
  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = bad_named_specs),
    "`specs` must be a non-empty named list.",
    fixed = TRUE
  )

  bad_empty_name_specs <- list(P1 = list(x = 1))
  names(bad_empty_name_specs)[1] <- ""
  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = bad_empty_name_specs),
    "Each entry in `specs` must have a non-empty name matching a facet value.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P4 = list(x = 1))),
    "Unknown facet key(s) in `specs`: P4.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P1 = list(1, 2, 3))),
    "Each line specification must be a named list.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P1 = list(list(y = 1)))),
    "Each line specification must include `x`.",
    fixed = TRUE
  )

  expect_error(
    ggsced_prep_lines(data = data_set, facet_col = "Participant", specs = list(P1 = list())),
    "No line rows were produced. Check `specs` and `facet_col` values.",
    fixed = TRUE
  )
})
