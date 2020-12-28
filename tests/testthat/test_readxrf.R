library(xrfr)

test_that("output is correct", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C.Int = c(2.38, 3.28, 2.13, 1.75),
                  N.Int = c(1.12, 0.43, 0.83, 1.29),
                  O.Int = c(1.29, 2.12, 1.92, 1.63))
  b <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)
  c <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C.Int = c(2.38, 3.28, 2.13, 1.75),
                  N.Int = c(1.12, 0.43, 0.83, 1.29),
                  O.Int = c(1.29, 2.12, 1.92, 1.63),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)

  ab <- readxrf(raw.data = a, project.info = b)

  expect_s3_class(ab, "data.frame")
  expect_equal(nrow(ab), 4)
  expect_equal(ncol(ab), 10)
  expect_identical(ab, c)
})


test_that("warnings and errors work", {
  a <- data.frame(Sample = c("A1", "A2", "A3", "A4"),
                  Date = "01.01.2020",
                  C.Int = c(2.38, 3.28, 2.13, 1.75),
                  N.Int = c(1.12, 0.43, 0.83, 1.29),
                  O.Int = c(1.29, 2.12, 1.92, 1.63))
  b <- data.frame(Sample = c("A1", "A2", "A3", "A5"),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)
  c <- data.frame(Smaple = c("A1", "A2", "A3", "A4"),
                  Filter_type = "PC",
                  Filter_size = 0.8,
                  Filter_box_nr = 1,
                  Filter_blank = c(NA, NA, NA, "blank"),
                  Volume = 1000)

  expect_warning(readxrf(raw.data = a, project.info = b))
  expect_error(readxrf(raw.data = a, project.info = c))
})
