context("Imports")

library(xrfr)
library(readxl)
library(assertr)

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

