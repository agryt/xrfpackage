library(xrfr)

test_that("output is correct", {
  a <- data.frame(Sample = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, NA, NA, NA, "blank", "blank", "blank"),
                  Volume = 1000,
                  Element = c("C", "N", "O", "C", "N", "O", "C", "N", "O"),
                  Concentration = c(2.86, -3.82, -0.603, 6.94, -19.3, 0.869, 0, 0, 0),
                  Detection_limit = c(1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32))
  b <- widen_above(project_data = a)
  c <- data.frame(Sample = c("A1", "A2"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = NA,
                  Volume = 1000,
                  C = c(2.86, 6.94),
                  O = c(NA, 0.869))

  expect_s3_class(b, "data.frame")
  expect_equal(ncol(b), 9)
  expect_equal(nrow(b), 2)
  expect_equal(dim(b), dim(c))
  expect_equal(names(b), names(c))
})
