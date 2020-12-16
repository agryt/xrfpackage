context("importxrf")

library(xrfr)

test_that("result is a dataframe", {
  a <- importxrf(raw.data = "testdata_rawdata.txt", project.info = "testdata_infofile.xlsx")

  expect_s3_class(a, "data.frame")
})
