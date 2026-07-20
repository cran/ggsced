test_that("ggsced runs and emits messages as normal", {
  test_data_frame = data.frame(Participant = c("A", "B", "C"),
                               x = c(1, 2, 3),
                               y = c(0, 0, 0))

  p = ggplot2::ggplot(test_data_frame,
                      ggplot2::aes(x, y)) +
    ggplot2::facet_wrap(~Participant)
  staggered_pls = list('1' = c(1.5,  2.5, 3.5))

  expect_no_error(ggsced(p, legs = staggered_pls))
  expect_no_warning(ggsced(p, legs = staggered_pls))
  expect_no_message(ggsced(p, legs = staggered_pls))
  expect_message(ggsced(p, legs = staggered_pls, verbose = TRUE))

  staggered_pls = list('1' = c(1.5,  2.5, 3.5),
                       '1' = c(1.5,  2.5, 3.5))

  staggered_offs = list(
    '1' = c(F, F, F),
    '2' = c(F, T, F))

  expect_no_error(ggsced(p, legs = staggered_pls, offs = staggered_offs))
  expect_no_warning(ggsced(p, legs = staggered_pls, offs = staggered_offs))
  expect_no_message(ggsced(p, legs = staggered_pls, offs = staggered_offs))
  expect_message(ggsced(p, legs = staggered_pls, offs = staggered_offs, verbose = TRUE))
})

test_that("ggsced should throw with bad plot object", {
  staggered_pls = list('1' = c(1.5,  2.5, 3.5))
  expect_error(ggsced(NULL, staggered_pls),
               "Plot object must be a valid ggplot object.")

  p <- c(1)
  expect_error(ggsced(p, staggered_pls),
               "Plot object must be a valid ggplot object.")
})

test_that("ggsced should throw with bad phase change list", {
  test_data_frame = data.frame(Participant = c("A", "B", "C"),
                               x = c(1, 2, 3),
                               y = c(0, 0, 0))

  p = ggplot2::ggplot(test_data_frame,
                      ggplot2::aes(x, y)) +
    ggplot2::facet_wrap(~Participant)

  expect_error(ggsced(p, legs = NA), "Phase change points must be a valid ordered list.")
  expect_error(ggsced(p, legs = NULL), "Phase change points must be a valid ordered list.")

  staggered_pls_bad = list('1' = c(1.5,  2.5, 3.5),
                           '2' = c(1.5,  2.5))

  expect_error(ggsced(p, staggered_pls_bad),
               "Phase change vectors in list are not of a uniform length.")

  staggered_pls_bad = list('1' = c(1.5,  2.5, 3.5),
                           '2' = c(1.5,  2.5, '3.5'))

  expect_error(ggsced(p, staggered_pls_bad))
})
