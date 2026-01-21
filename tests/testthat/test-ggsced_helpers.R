test_that("Correct facet labels are generated", {
  data_set = Gilroyetal2015

  y_mult = .05
  x_mult = .02

  p = ggplot(data_set, aes(Session, Responding, group = Condition)) +
    geom_line() +
    geom_point(size = 3) +
    scale_y_continuous(name = "Percentage Accuracy",
                       limits = c(0, 100),
                       breaks = (0:4) * 25,
                       expand = expansion(mult = c(y_mult))) +
    scale_x_continuous(breaks = c(1:27),
                       limits = c(1, 27),
                       expand = expansion(mult = c(x_mult))) +
    facet_grid2(Participant ~ .,
                remove_labels = "x",
                axes = "x") +
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
  data_set = Gilroyetal2015

  y_mult = .05
  x_mult = .02

  p = ggplot(data_set, aes(Session, Responding, group = Condition)) +
    geom_line() +
    geom_point(size = 3) +
    scale_y_continuous(name = "Percentage Accuracy",
                       limits = c(0, 100),
                       breaks = (0:4) * 25,
                       expand = expansion(mult = c(y_mult))) +
    scale_x_continuous(breaks = c(1:27),
                       limits = c(1, 27),
                       expand = expansion(mult = c(x_mult))) +
    facet_grid2(Participant ~ .,
                remove_labels = "x",
                axes = "x") +
    theme(text = element_text(size = 14, color = 'black'),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    ggsced_style_x(x_mult, lwd = 2) +
    ggsced_style_y(y_mult, lwd = 2)

  simple_condition_labels_df = ggsced_condition_labels(p)

  true_result_df = data.frame(
    Session = c(2.5, 24, 18.5, 7.5),
    Responding = c(100, 100, 100, 100),
    Condition = c(1, 1, 1, 1),
    Participant = factor(c("Andrew", "Andrew", "Andrew", "Andrew"),
                         levels = c("Andrew", "Brian", "Charles")),
    label = c("Baseline", "Generalization", "Maintenance", "Treatment")
  )

  expect_identical(simple_condition_labels_df, true_result_df)
})
