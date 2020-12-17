
projectfile.df <- importxrf(raw.data = "xrf_rawdata.txt", project.info = "xrf_projectinfo.xlsx")

project.df <- convertxrf(imported.data = projectfile.df, setup = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")

wide.df <- widen(project.data = project.df)

wideabove.df <- widen_above(project.data = project.df)

widemeans.df <- widen_means(project.data = project.df, first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")

widemeansabove.df <- widen_means_above(project.data = project.df, first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")




a <- importdata(raw.data = system.file("/tests/testthat", "testdata_rawdata.txt", package = "xrfr"))
b <- importinfo(project.info = system.file("/tests/testthat", "testdata_infofile.xlsx", package = "xrfr"))

c <- importxrf(raw.data = system.file("/tests/testthat", "testdata_rawdata.txt", package = "xrfr"), project.info = system.file("/tests/testthat", "testdata_infofile.xlsx", package = "xrfr"))
