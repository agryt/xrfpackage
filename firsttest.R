rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
setup.df <- read_excel("xrf_setup.xlsx")

projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)

project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")



wide.df <- widen(project.data = project.df)

wideabove.df <- widen_above(project.data = project.df)

widemeans.df <- widen_means(project.data = project.df, first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")

widemeansabove.df <- widen_means_above(project.data = project.df, first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")
