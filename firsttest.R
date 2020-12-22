rawdata.df <- read_delim(system.file("extdata", "xrf_rawdata.txt", package = "xrfr"), delim = "\t", locale = readr::locale(decimal_mark = ","))
projectinfo.df <- read_excel(system.file("extdata", "xrf_projectinfo.xlsx", package = "xrfr"))
setup.df <- read_excel(system.file("extdata", "xrf_setup.xlsx", package = "xrfr"))

projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)

project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")



wide.df <- widen(project.data = project.df)

wideabove.df <- widen_above(project.data = project.df)

widemeans2.df <- widen_means(project.data = project.df, first_factor = "Day", second_factor = "Treatment")

widemeansabove.df <- widen_means_above(project.data = project.df, first_factor = "Day", second_factor = "Treatment")
