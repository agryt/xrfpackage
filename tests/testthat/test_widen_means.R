library(xrfr)

test_that("output is correct with one factor", {
  a <- data.frame(Sample = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3", "A4", "A4", "A4", "A5", "A5", "A5"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "blank", "blank", "blank"),
                  Volume = 1000,
                  Treatment = c("X", "X", "X", "X", "X", "X", "Z", "Z", "Z", "Z", "Z", "Z", NA, NA, NA),
                  Element = c("C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O"),
                  Concentration = c(2.86, -3.82, -0.603, 6.94, -19.3, 0.869, 3.86, -1.82, -0.873, 6.27, -12.8, 0.592, 0, 0, 0),
                  Detection_limit = c(1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32))
  b <- widen_means(project.data = a, first_factor = "Treatment")
  c <- data.frame(Treatment = c("X", "Z"),
                  Volume = 1000,
                  C = c(4.900, 5.065),
                  N = c(-11.56, -7.31),
                  O = c(0.1330, -0.1405))

  expect_s3_class(b, "data.frame")
  expect_equal(ncol(b), 5)
  expect_equal(nrow(b), 2)
  expect_equal(dim(b), dim(c))
  expect_equal(names(b), names(c))
})

test_that("output is correct with two factors", {
  a <- data.frame(Sample = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3", "A4", "A4", "A4", "A5", "A5", "A5"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "blank", "blank", "blank"),
                  Volume = 1000,
                  Treatment = c("X", "X", "X", "X", "X", "X", "Z", "Z", "Z", "Z", "Z", "Z", NA, NA, NA),
                  Time = c(0, 0, 0, 2, 2, 2, 0, 0, 0, 2, 2, 2, NA, NA, NA),
                  Element = c("C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O"),
                  Concentration = c(2.86, -3.82, -0.603, 6.94, -19.3, 0.869, 3.86, -1.82, -0.873, 6.27, -12.8, 0.592, 0, 0, 0),
                  Detection_limit = c(1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32))
  b <- widen_means(project.data = a, first_factor = "Treatment", second_factor = "Time")
  c <- data.frame(Treatment = c("X", "X", "Z", "Z"),
                  Time = c(0, 2, 0, 2),
                  Volume = 1000,
                  C = c(2.86, 6.94, 3.86, 6.27),
                  N = c(-3.82, -19.30, -1.82, -12.80),
                  O = c(-0.603, 0.869, -0.873, 0.592))

  expect_s3_class(a, "data.frame")
  expect_equal(ncol(b), 6)
  expect_equal(nrow(b), 4)
  expect_equal(dim(b), dim(c))
  expect_equal(names(b), names(c))
})
