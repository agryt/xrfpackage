
projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")

wide.df <- widen(projectpath = "project_long.csv")

wideabove.df <- widen_above(projectpath = "project_long.csv")

widemeans.df <- widen_means(projectpath = "project_long.csv", first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")

widemeansabove.df <- widen_means_above(projectpath = "project_long.csv", first_factor = "Day", second_factor = "Treatment", first_element = "C", last_element = "As")




projectfile.df <- importxrf(datapath = "2ANS&2AEN_merged.txt", infopath = "Infofile_forR.xlsx")

project1.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")



