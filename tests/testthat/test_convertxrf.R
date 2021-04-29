library(xrfr)

test_that("output is correct", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C = c(2.38, 3.28, 2.13, 1.75),
                  N = c(1.12, 0.43, 0.83, 1.29),
                  O = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)
  b <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  ANO = c(0.003, 0.043, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  MolarW = c(12.01, 14.01, 16),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_ANO = c(0.82, 0.32, 0.19),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2008 = c(36, 13, 5),
                  Drift_2019 = c(36, 12, 4))
  ab <- convertxrf(imported_data = a, base_info = b, year = "2019", first_element = "C", last_element = "O")
  c <- data.frame(Sample = c("A1", "A1", "A1", "A2", "A2", "A2", "A3", "A3", "A3", "A4", "A4", "A4"),
                  Date = "01.01.2020",
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "blank", "blank", "blank"),
                  Volume = 1000,
                  Element = c("C", "N", "O", "C", "N", "O", "C", "N", "O", "C", "N", "O"),
                  Concentration = c(2.86, -3.82, -0.603, 6.94, -19.3, 0.869, 1.72, -10.3, 0.514, 0, 0, 0),
                  Adjusted_detection_limit = c(1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32, 1.22, 0.31, 0.32))

  expect_s3_class(ab, "data.frame")
  expect_equal(nrow(ab), 12)
  expect_equal(ncol(ab), 10)
  expect_equal(dim(ab), dim(c))
  expect_equal(names(ab), names(c))
})


test_that("blank error works", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C = c(2.38, 3.28, 2.13, 1.75),
                  N = c(1.12, 0.43, 0.83, 1.29),
                  O = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = NA,
                  Volume = 1000)
  b <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  ANO = c(0.003, 0.043, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  MolarW = c(12.01, 14.01, 16),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_ANO = c(0.82, 0.32, 0.19),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2008 = c(36, 13, 5),
                  Drift_2019 = c(36, 12, 4))

  expect_error(convertxrf(imported_data = a, base_info = b, year = "2019", first_element = "C", last_element = "O"))
})

test_that("column names error works", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C = c(2.38, 3.28, 2.13, 1.75),
                  N = c(1.12, 0.43, 0.83, 1.29),
                  O = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)
  b <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  MolarW = c(12.01, 14.01, 16),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_ANO = c(0.82, 0.32, 0.19),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2008 = c(36, 13, 5),
                  Drift_2019 = c(36, 12, 4))
  c <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  ANO = c(0.003, 0.043, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_ANO = c(0.82, 0.32, 0.19),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2008 = c(36, 13, 5),
                  Drift_2019 = c(36, 12, 4))
  d <- data.frame(Element = c("C", "N", "O"),
                  PC = c(0.006, 0.032, 0.0025),
                  GFF = c(0.0094, 0.0345, 0.0025),
                  MolarW = c(12.01, 14.01, 16),
                  DL_PC = c(1.22, 0.31, 0.32),
                  DL_ANO = c(0.82, 0.32, 0.19),
                  DL_GFF = c(0.13, 0.2, 0.15),
                  Drift_2019 = c(36, 12, 4))

  expect_error(convertxrf(imported_data = a, base_info = b, year = "2019", first_element = "C", last_element = "O"))
  expect_error(convertxrf(imported_data = a, base_info = c, year = "2019", first_element = "C", last_element = "O"))
  expect_error(convertxrf(imported_data = a, base_info = d, year = "2019", first_element = "C", last_element = "O"))
})
