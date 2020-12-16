context("Imports")

library(xrfr)

test_that("import is a dataframe", {
  a <- importdata(raw.data = "testdata_rawdata.txt")
  b <- importinfo(project.info = "testdata_infofile.xlsx")
  c <- importsetup(setup = "testdata_setup.xlsx")

  expect_s3_class(a, "data.frame")
  expect_s3_class(b, "data.frame")
  expect_s3_class(c, "data.frame")
})


test_that("import functions work the same as the code", {
  a1 <- importdata(raw.data = "testdata_rawdata.txt")
  b1 <- importinfo(project.info = "testdata_infofile.xlsx")
  c1 <- importsetup(setup = "testdata_setup.xlsx")

  datafile <- readr::read_delim("testdata_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
  setupfile <- readxl::read_excel("testdata_setup.xlsx")

  a2 <- datafile %>%
    dplyr::select(c(Sample, Date, dplyr::contains("Int"))) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")
  b2 <- readxl::read_excel("testdata_infofile.xlsx")
  c2 <- setupfile %>%
    tidyr::pivot_longer(PC:GFF, names_to = "Filter_type", values_to = "Cal_const") %>%
    dplyr::relocate(Filter_type)

  expect_identical(a1, a2)
  expect_identical(b1, b2)
  expect_identical(c1, c2)
})

test_that("imported dataframes match created dataframes", {
  a1 <- importdata(raw.data = "testdata_rawdata.txt")
  a1 <- as.data.frame(a1)
  b1 <- importinfo(project.info = "testdata_infofile.xlsx")
  b1 <- as.data.frame(b1)
  c1 <- importsetup(setup = "testdata_setup.xlsx")
  c1 <- as.data.frame(c1)

  a2 <- data.frame(Sample = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"),
                        Date = c("01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020", "01.01.2020"),
                        C = c(54.98752, 56.42987, 53.24936, 51.42387, 53.23487, 59.32487, 51.23871, 59.23092, 48.23981, 54.09813),
                        N = c(0.287509, 0.698235, 0.398389, 0.736982, 0.348098, 0.398092, 0.607234, 0.459872, 0.409709, 0.398092),
                        O = c(3.348709, 2.487923, 2.589723, 3.134987, 2.598702, 2.450982, 3.287924, 1.908322, 2.234987, 2.123098),
                        Na = c(0.345987, 0.909342, 1.435098, 1.435873, 1.230395, 0.982345, 1.245985, 0.120985, 1.008234, 1.029341),
                        Mg = c(1.245098, 3.245809, 2.240598, 1.245982, 3.234982, 1.234872, 2.234098, 3.234872, 1.872345, 1.098324))
  b2 <- data.frame(Sample = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"),
                   Filter_type = c("PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC", "PC"),
                   Filter_size = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
                   Filter_box_nr = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                   Filter_blank = c(NA, NA, NA, NA, NA, NA, NA, NA, "blank", "blank"),
                   Volume = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000),
                   Location = c("L1", "L1", "L1", "L1", "L2", "L2", "L2", "L2", NA, NA),
                   Depth = c("10m", "50m", "100m", "200m", "10m", "50m", "100m", "200m", NA, NA))
  c2 <- data.frame(Filter_type = c("PC", "ANO", "GFF", "PC", "ANO", "GFF", "PC", "ANO", "GFF", "PC", "ANO", "GFF", "PC", "ANO", "GFF"),
                   Element = c("C", "C", "C", "N", "N", "N", "O", "O", "O", "Na", "Na", "Na", "Mg", "Mg", "Mg"),
                   MolarW = c(12.01, 12.01, 12.01, 14.01, 14.01, 14.01, 16.00, 16.00, 16.00, 22.99, 22.99, 22.99, 24.31, 24.31, 24.31),
                   DL_PC = c(1.221938021, 1.221938021, 1.221938021, 0.312338639, 0.312338639, 0.312338639, 0.032214656, 0.032214656, 0.032214656, 0.036253907, 0.036253907, 0.036253907, 0.004652646, 0.004652646, 0.004652646),
                   DL_ANO = c(0.113231162, 0.113231162, 0.113231162, 0.061264709, 0.061264709, 0.061264709, 0.209267286, 0.209267286, 0.209267286, 0.010594526, 0.010594526, 0.010594526, 0.004313552, 0.004313552, 0.004313552),
                   DL_GFF = c(0.1316474263, 0.1316474263, 0.1316474263, 0.2076325408, 0.2076325408, 0.2076325408, 0.1563397707, 0.1563397707, 0.1563397707, 0.0961809833, 0.0961809833, 0.0961809833, 0.0287938984, 0.0287938984, 0.0287938984),
                   Drift_2008 = c(36.18, 36.18, 36.18, 13.15, 13.15, 13.15, 5.01, 5.01, 5.01, 37.10, 37.10, 37.10, 73.70, 73.70, 73.70),
                   Drift_2010 = c(36.00, 36.00, 36.00, 13.20, 13.20, 13.20, 4.96, 4.96, 4.96, 37.61, 37.61, 37.61, 74.65, 74.65, 74.65),
                   Drift_2012 = c(34.49, 34.49, 34.49, 12.08, 12.08, 12.08, 4.46, 4.46, 4.46, 32.82, 32.82, 32.82, 64.54, 64.54, 64.54),
                   Drift_2013 = c(23.43, 23.43, 23.43, 8.42, 8.42, 8.42, 3.36, 3.36, 3.36, 23.40, 23.40, 23.40, 49.72, 49.72, 49.72),
                   Drift_2014 = c(34.56, 34.56, 34.56, 12.30, 12.30, 12.30, 4.60, 4.60, 4.60, 32.51, 32.51, 32.51, 68.45, 68.45, 68.45),
                   Drift_2015 = c(34.57, 34.57, 34.57, 8.65, 8.65, 8.65, 1.98, 1.98, 1.98, 23.45, 23.45, 23.45, 61.15, 61.15, 61.15),
                   Drift_2017 = c(35.70, 35.70, 35.70, 12.32, 12.32, 12.32, 4.68, 4.68, 4.68, 35.91, 35.91, 35.91, 71.78, 71.78, 71.78),
                   Drift_2018 = c(36.00, 36.00, 36.00, 12.42, 12.42, 12.42, 4.30, 4.30, 4.30, 35.94, 35.94, 35.94, 71.93, 71.93, 71.93),
                   Drift_2019 = c(36.20, 36.20, 36.20, 12.42, 12.42, 12.42, 4.72, 4.72, 4.72, 35.94, 35.94, 35.94, 72.11, 72.11, 72.11),
                   Cal_const = c(0.0060000000, 0.0034000000, 0.0094000000, 0.0320000000, 0.0136000000, 0.0345000000, 0.0025000000, 0.0025000000, 0.0025000000, 0.0001460000, 0.0001460000, 0.0001460000, 0.0000800000, 0.0000800000, 0.0000800000))

  expect_identical(dim(a1), dim(a2))
  expect_identical(dim(b1), dim(b2))
  expect_identical(dim(c1), dim(c2))
})
