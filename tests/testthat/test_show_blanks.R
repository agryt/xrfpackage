library(xrfr)

test_that("output is correct", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C = c(2.38, 3.28, 2.13, 1.75),
                  N = c(1.12, 0.43, 0.83, 1.29),
                  O = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = c("PC", "GFF", "PC", "GFF"),
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, "blank", "blank"),
                  Volume = 1000)
  b <- show_blanks(imported_data = a, first_element = "C", last_element = "O")
  c <- data.frame(Filter_type = c("PC", "GFF"),
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  C = c(2.13, 1.75),
                  N = c(0.83, 1.29),
                  O = c(1.92, 1.63))

  expect_s3_class(b, "data.frame")
  expect_equal(nrow(b), 2)
  expect_equal(ncol(b), 6)
  expect_equal(dim(b), dim(c))
  expect_equal(names(b), names(c))
})
