library(xrfr)

test_that("output is correct", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C.Int = c(2.38, 3.28, 2.13, 1.75),
                  N.Int = c(1.12, 0.43, 0.83, 1.29),
                  O.Int = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)
  colnames(a) <- c("Sample", "Date", "C", "N", "O", "Filter_type", "Filter_size", "Filter_box_nr", "Filter_blank", "Volume")
  b <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  MolarW = c(12.01, 14.01, 16),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2008 = c(36, 13, 5),
                  Drift_2019 = c(36, 12, 4))
  ab <- convertxrf(imported.data = a, setup = b, year = "2019", first_element = "C", last_element = "O")
  c <- data.frame(Sample = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3", "A4", "A4", "A4"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "blank", "blank", "blank"),
                  Volume = 1000,
                  Element = c("C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O"),
                  Concentration = c(2.86, -3.82, -0.603, 6.94, -19.3, 0.869, 1.72, -10.3, 0.514, 0, 0, 0),
                  Detection_limit = c(1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32))

  expect_s3_class(ab, "data.frame")
  expect_equal(nrow(ab), 12)
  expect_equal(ncol(ab), 10)
  expect_equal(dim(ab), dim(c))
  expect_equal(names(ab), names(c))
})

test_that("code stops running when supposed to", {

})
